;;; gptel-vterm.el --- Let gptel put commands on your vterm prompt  -*- lexical-binding: t; -*-

;; Version: 0.3.0
;; Package-Requires: ((emacs "29.1") (gptel "0.9.0") (vterm "0.0.2"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Registers one gptel tool, "vterm_type", which writes a command onto the
;; input line of a vterm session WITHOUT running it.  You review or edit the
;; text and press Enter yourself.  The tool cannot execute anything, and it
;; cannot read the terminal, so the model never sees your output.
;;
;; Setup:
;;
;;   (require 'gptel-vterm)
;;   (setopt gptel-use-tools t)
;;
;; Enable "vterm_type" in `gptel-menu' (`gptel-tools'), or use a preset:
;;
;;   (gptel-make-preset 'terminal :tools '("vterm_type"))
;;
;; Safety
;; ------
;; The text must be a single line free of control characters (newline, CR,
;; tab, ESC, DEL, C1), so nothing the model sends can submit the line or
;; trigger completion.  Bracketed paste is deliberately not used: shells
;; without it would execute pasted newlines.  By default the input line is
;; cleared first (C-e C-u; bash/zsh/readline) so a stale suggestion is
;; replaced instead of concatenated; the model can pass `append' to keep it.
;;
;; Limits: use it only when the session sits at a shell prompt.  If a
;; program is running in the foreground, the text goes to that program.
;; The tool cannot tell, because it does not read the terminal.

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

(defcustom gptel-vterm-type-action 'display
  "What to do with the session buffer after text was written to it.
`display' shows it without selecting (no focus stealing, so stray key
presses cannot reach the terminal); `select' pops to it so Enter is one
keypress away; nil leaves windows alone."
  :type '(choice (const :tag "Display, keep focus" display)
                 (const :tag "Select the terminal" select)
                 (const :tag "Do nothing" nil)))

(defcustom gptel-vterm-confirm nil
  "Whether to ask before the tool writes to the terminal.
It never executes anything, so the default is not to ask.  Read at
registration time; call `gptel-vterm-register' after changing it."
  :type 'boolean)

(defcustom gptel-vterm-category "vterm"
  "gptel tool category used for the registered tool."
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
  "Create and return a new session buffer NAME."
  (let* ((shell (or gptel-vterm-shell vterm-shell))
         (dir (file-name-as-directory
               (expand-file-name
                (or gptel-vterm-default-directory default-directory))))
         (buf (generate-new-buffer (gptel-vterm--buffer-name name))))
    (with-current-buffer buf
      (setq default-directory dir)
      (let ((vterm-shell shell))
        (vterm-mode))
      ;; Set after the major mode, which resets buffer-local variables.
      (setq gptel-vterm--name name)
      (when-let* ((proc (get-buffer-process buf)))
        (set-process-query-on-exit-flag proc nil)))
    buf))

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

(defun gptel-vterm--present (buf)
  "Make BUF visible per `gptel-vterm-type-action'."
  (pcase gptel-vterm-type-action
    ('select (pop-to-buffer buf))
    ('display (display-buffer buf))))

;;;; The tool

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
                        (with-current-buffer buf
                          (unless append
                            (vterm-send-key "e" nil nil t) ; C-e
                            (vterm-send-key "u" nil nil t)) ; C-u
                          (vterm-send-string text))
                        (gptel-vterm--present buf)
                        (funcall callback
                                 (format "Wrote the command to session `%s' WITHOUT executing it; Enter was not pressed. The user will review it and press Enter themselves. You cannot see the result, so do not assume it ran; ask the user for the output if you need it."
                                         name)))
                    (error (funcall callback
                                    (format "Error: %s"
                                            (error-message-string err2))))))))
          (if existing
              (funcall write)
            (gptel-vterm--when-ready buf write))))
    (error (funcall callback (format "Error: %s" (error-message-string err))))))

;;;; Registration

(defvar gptel-vterm-tools nil
  "The `gptel-tool' objects created by `gptel-vterm-register'.")

;;;###autoload
(defun gptel-vterm-register ()
  "(Re)register the vterm_type tool with gptel."
  (interactive)
  (setq gptel-vterm-tools
        (list
         (gptel-make-tool
          :name "vterm_type"
          :category gptel-vterm-category
          :async t :include t
          :confirm gptel-vterm-confirm
          :function #'gptel-vterm--type
          :description "Write a shell command onto the input line of the user's terminal WITHOUT running it. Enter is never pressed and this tool cannot execute anything: the user sees the text in their terminal, reviews or edits it, and presses Enter themselves. You cannot read the terminal or its output; ask the user if you need to know the result. Use it to propose commands for the user to run. Single line only (control characters are rejected); chain with && or ; when needed. Replaces whatever is currently on the input line unless `append` is true. Only works when the terminal is at a shell prompt. Never claim the command ran."
          :args (list
                 '( :name "command" :type string
                    :description "The single-line command to put on the prompt.")
                 '( :name "session" :type string :optional t
                    :description "Terminal session name (letters, digits, - and _). Defaults to \"main\". Created if it does not exist.")
                 '( :name "append" :type boolean :optional t
                    :description "Keep text already on the input line and append to it (default: replace).")))))
  gptel-vterm-tools)

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
