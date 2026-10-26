;;; gptel-vterm.el --- Let gptel put commands on your vterm prompt  -*- lexical-binding: t; -*-

;; Version: 0.4.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.0") (vterm "0.0.2"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Registers two gptel tools working on ordinary vterm buffers:
;;
;;   vterm_type  Writes a command onto the input line WITHOUT running it.
;;               You review or edit it and press Enter yourself.  It cannot
;;               execute anything.
;;   vterm_read  Returns the last lines of the terminal, so the model can see
;;               the result after you ran the command.  Asks for confirmation
;;               by default, because it exposes terminal contents.
;;
;; The sessions are normal vterm buffers (created through `vterm' itself):
;; you can type in them, split them, and use them like any other terminal.
;;
;; Setup:
;;
;;   (require 'gptel-vterm)
;;   (setopt gptel-use-tools t)
;;
;; Enable the tools in `gptel-menu' (`gptel-tools'), or use a preset:
;;
;;   (gptel-make-preset 'terminal :tools '("vterm_type" "vterm_read"))
;;
;; Safety of vterm_type
;; --------------------
;; The text must be a single line free of control characters (newline, CR,
;; tab, ESC, DEL, C1), so nothing the model sends can submit the line or
;; trigger completion.  Bracketed paste is deliberately not used: shells
;; without it would execute pasted newlines.  By default the input line is
;; cleared first (C-e C-u; bash/zsh/readline) so a stale suggestion is
;; replaced instead of concatenated; the model can pass `append' to keep it.
;;
;; Limits: use vterm_type only when the session sits at a shell prompt.  If
;; a program runs in the foreground, the text goes to that program.  There
;; is no way to tell, and vterm_read cannot say whether a command finished;
;; it only shows the screen and recent scrollback.  Whatever is in the
;; terminal (secrets included) is sent to the model when it reads.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'vterm)

;;;; Customization

(defgroup gptel-vterm nil
  "Put gptel-suggested commands on a vterm prompt."
  :group 'gptel
  :prefix "gptel-vterm-")

(defcustom gptel-vterm-shell nil
  "Shell for new sessions, or nil to use `vterm-shell'."
  :type '(choice (const :tag "Use `vterm-shell'" nil) string))

(defcustom gptel-vterm-default-directory nil
  "Directory new sessions start in, or nil for `default-directory'."
  :type '(choice (const nil) directory))

(defcustom gptel-vterm-startup-timeout 5
  "Seconds to wait for a new shell to print its first output."
  :type 'number)

(defcustom gptel-vterm-type-action 'select
  "What to do with the session buffer after text was written to it.
`select' pops to it so you can review the text and press Enter right
away; `display' shows it without taking focus; nil leaves windows alone."
  :type '(choice (const :tag "Select the terminal" select)
                 (const :tag "Display, keep focus" display)
                 (const :tag "Do nothing" nil)))

(defcustom gptel-vterm-read-lines 60
  "Default number of lines returned by `vterm_read'."
  :type 'integer)

(defcustom gptel-vterm-max-output 20000
  "Maximum characters returned by `vterm_read'.
Longer output keeps the head and (mostly) the tail."
  :type 'integer)

(defcustom gptel-vterm-confirm nil
  "Whether to ask before `vterm_type' writes to the terminal.
It never executes anything, so the default is not to ask.  Read at
registration time; call `gptel-vterm-register' after changing it."
  :type 'boolean)

(defcustom gptel-vterm-confirm-read t
  "Whether to ask before `vterm_read' sends terminal contents to the model.
Read at registration time; call `gptel-vterm-register' after changing it."
  :type 'boolean)

(defcustom gptel-vterm-category "vterm"
  "gptel tool category used for the registered tools."
  :type 'string)

;;;; Sessions

(defconst gptel-vterm--buffer-prefix "*gptel-vterm:")

(defvar-local gptel-vterm--name nil
  "Session name; non-nil marks a buffer managed by this package.")

(defun gptel-vterm--session-name (session)
  "Validate and return SESSION, defaulting to \"main\"."
  (let ((name (if (and (stringp session) (not (string-blank-p session)))
                  (string-trim session)
                "main")))
    (unless (string-match-p "\\`[A-Za-z0-9_-]\\{1,32\\}\\'" name)
      (user-error "Invalid session name %S (letters, digits, - and _ only)"
                  name))
    name))

(defun gptel-vterm--buffer-name (name)
  (format "%s%s*" gptel-vterm--buffer-prefix name))

(defun gptel-vterm--find (name)
  "Return the live session buffer called NAME, or nil."
  (let ((buf (get-buffer (gptel-vterm--buffer-name name))))
    (cond ((null buf) nil)
          ((process-live-p (get-buffer-process buf)) buf)
          (t (let (kill-buffer-query-functions) (kill-buffer buf)) nil))))

(defun gptel-vterm--session-names ()
  (delq nil (mapcar (lambda (b) (buffer-local-value 'gptel-vterm--name b))
                    (buffer-list))))

(defun gptel-vterm--create (name)
  "Create a regular vterm buffer for session NAME and return it.
Goes through `vterm' itself so the buffer is set up exactly like one you
would start by hand; the window layout is left untouched here."
  (let* ((bufname (gptel-vterm--buffer-name name))
         (default-directory
          (file-name-as-directory
           (expand-file-name
            (or gptel-vterm-default-directory default-directory))))
         (vterm-shell (or gptel-vterm-shell vterm-shell)))
    (save-window-excursion (vterm bufname))
    (let ((buf (get-buffer bufname)))
      (unless buf (error "Could not create vterm session `%s'" name))
      (with-current-buffer buf
        (setq gptel-vterm--name name)
        (when-let* ((proc (get-buffer-process buf)))
          (set-process-query-on-exit-flag proc nil)))
      buf)))

(defun gptel-vterm--ready-p (buf)
  "Non-nil once the shell in BUF has printed something (its prompt)."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward "[^ \t\n]" nil t) t))))

(defun gptel-vterm--when-ready (buf callback)
  "Call CALLBACK once BUF's shell shows a prompt, or after the timeout."
  (let ((deadline (time-add (current-time) gptel-vterm-startup-timeout)))
    (cl-labels ((poll ()
                  (if (or (not (buffer-live-p buf))
                          (gptel-vterm--ready-p buf)
                          (time-less-p deadline (current-time)))
                      (funcall callback)
                    (run-with-timer 0.1 nil #'poll))))
      (poll))))

(defun gptel-vterm--prepare-input (buf)
  "Make sure BUF accepts typing: leave `vterm-copy-mode' (read-only)."
  (with-current-buffer buf
    (when (bound-and-true-p vterm-copy-mode)
      (vterm-copy-mode -1))
    (when buffer-read-only
      (setq buffer-read-only nil))))

(defun gptel-vterm--present (buf)
  "Make BUF visible per `gptel-vterm-type-action'."
  (pcase gptel-vterm-type-action
    ('select (pop-to-buffer buf))
    ('display (display-buffer buf))))

;;;; Tool: vterm_type (writes, never executes)

(defun gptel-vterm--type (callback command &optional session append)
  "Write COMMAND at the prompt of SESSION and never press Enter.
Asynchronous tool function: the result goes to CALLBACK.  With APPEND,
keep any text already on the input line."
  (condition-case err
      (let* ((text (and (stringp command)
                        (string-trim-right command "[\n\r]+")))
             (append (and append (not (eq append :json-false))))
             (name (gptel-vterm--session-name session)))
        (when (or (null text) (string-blank-p text))
          (user-error "Empty command"))
        ;; Reject anything that could submit or alter the line: newline, CR,
        ;; tab (completion), ESC and other C0 controls, DEL, C1 controls.
        (when (string-match-p "[[:cntrl:]\u007f-\u009f]" text)
          (user-error
           "Text must be a single line without control characters, so that it can never execute by itself"))
        (let* ((existing (gptel-vterm--find name))
               (buf (or existing (gptel-vterm--create name)))
               (write
                (lambda ()
                  (condition-case err2
                      (progn
                        (gptel-vterm--prepare-input buf)
                        (with-current-buffer buf
                          (unless append
                            (vterm-send-key "e" nil nil t)  ; C-e
                            (vterm-send-key "u" nil nil t)) ; C-u
                          (vterm-send-string text))
                        (gptel-vterm--present buf)
                        (funcall callback
                                 (format "Wrote the command to session `%s' WITHOUT executing it; Enter was not pressed. The user will review it and press Enter themselves. Do not assume it ran. Once the user says it did, use vterm_read to see the result."
                                         name)))
                    (error (funcall callback
                                    (format "Error: %s"
                                            (error-message-string err2))))))))
          (if existing
              (funcall write)
            (gptel-vterm--when-ready buf write))))
    (error (funcall callback (format "Error: %s" (error-message-string err))))))

;;;; Tool: vterm_read

(defun gptel-vterm--num (x default)
  "Coerce X (number or numeric string) to a number, else DEFAULT."
  (cond ((numberp x) x)
        ((and (stringp x) (string-match-p "\\`[0-9]+\\(?:\\.[0-9]+\\)?\\'" x))
         (string-to-number x))
        (t default)))

(defun gptel-vterm--clean (text)
  "Trim trailing blanks on each line and blank lines at both ends of TEXT."
  (string-trim
   (mapconcat #'string-trim-right (split-string text "\n") "\n")
   "\n+" "\n+"))

(defun gptel-vterm--truncate (s)
  (let ((max gptel-vterm-max-output)
        (len (length s)))
    (if (<= len max)
        s
      (let ((head (/ max 4)))
        (format "%s\n[... %d characters truncated ...]\n%s"
                (substring s 0 head)
                (- len max)
                (substring s (- len (- max head))))))))

(defun gptel-vterm--read (&optional session lines)
  "Tool function for `vterm_read': the last LINES lines of SESSION."
  (condition-case err
      (let* ((name (gptel-vterm--session-name session))
             (buf (or (gptel-vterm--find name)
                      (user-error "No session named `%s'" name)))
             (n (max 1 (min 500 (round (gptel-vterm--num
                                        lines gptel-vterm-read-lines))))))
        (with-current-buffer buf
          (let ((text
                 (save-excursion
                   (goto-char (point-max))
                   ;; Over-read: the screen below the cursor is blank padding.
                   (forward-line (- (+ n 300)))
                   ;; `filter-buffer-substring' drops vterm's soft-wrap
                   ;; newlines, so long lines come back unbroken.
                   (gptel-vterm--clean
                    (substring-no-properties
                     (filter-buffer-substring (point) (point-max)))))))
            (concat
             (format "[session %s, last %d lines; cannot tell whether a command is still running]\n"
                     name n)
             (gptel-vterm--truncate
              (mapconcat #'identity (last (split-string text "\n") n)
                         "\n"))))))
    (error (format "Error: %s" (error-message-string err)))))

;;;; Registration

(defvar gptel-vterm-tools nil
  "The `gptel-tool' objects created by `gptel-vterm-register'.")

;;;###autoload
(defun gptel-vterm-register ()
  "(Re)register the vterm tools with gptel."
  (interactive)
  (let ((session-arg
         '( :name "session" :type string :optional t
            :description "Terminal session name (letters, digits, - and _). Defaults to \"main\".")))
    (setq gptel-vterm-tools
          (list
           (gptel-make-tool
            :name "vterm_type"
            :category gptel-vterm-category
            :async t :include t
            :confirm gptel-vterm-confirm
            :function #'gptel-vterm--type
            :description "Write a shell command onto the input line of the user's terminal WITHOUT running it. Enter is never pressed and this tool cannot execute anything: the user sees the text in their terminal, reviews or edits it, and presses Enter themselves. Use it to propose commands for the user to run. Single line only (control characters are rejected); chain with && or ; when needed. Replaces whatever is currently on the input line unless `append` is true. Creates the session if it does not exist. Only works when the terminal is at a shell prompt. Never claim the command ran; after the user says they ran it, use vterm_read."
            :args (list
                   '( :name "command" :type string
                      :description "The single-line command to put on the prompt.")
                   (append session-arg nil)
                   '( :name "append" :type boolean :optional t
                      :description "Keep text already on the input line and append to it (default: replace).")))
           (gptel-make-tool
            :name "vterm_read"
            :category gptel-vterm-category
            :confirm gptel-vterm-confirm-read
            :function #'gptel-vterm--read
            :description "Read the most recent lines of the user's terminal session: use it after the user ran a command you proposed with vterm_type, to see its output and any errors. It shows the screen and recent scrollback as plain text; it cannot tell whether a command is still running, so if output looks incomplete, ask the user or read again later. Read-only."
            :args (list
                   (append session-arg nil)
                   '( :name "lines" :type integer :optional t
                      :description "How many trailing lines to return (default 60, max 500).")))))
    gptel-vterm-tools))

;;;###autoload
(defun gptel-vterm-show (name)
  "Pop to the gptel vterm session NAME to review a command and press Enter."
  (interactive
   (list (completing-read "Session: " (gptel-vterm--session-names) nil t)))
  (pop-to-buffer (or (gptel-vterm--find name)
                     (user-error "No session named `%s'" name))))

(gptel-vterm-register)

(provide 'gptel-vterm)
;;; gptel-vterm.el ends here
