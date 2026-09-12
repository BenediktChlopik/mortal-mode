;;; mortal-response-keymap.el --- which-key popups for response keymaps -*- lexical-binding: t; -*-

;; Author: You
;; Version: 1.0
;; Package-Requires: ((emacs "29.1") (which-key "3.0"))
;; Keywords: convenience, help

;;; Commentary:

;; Shows a which-key popup for "response keymaps" -- keymaps like
;; `query-replace-map' that are consulted by `read-key' loops such as
;; `perform-replace', `y-or-n-p', and `map-y-or-n-p', rather than by
;; Emacs's normal `read-key-sequence' prefix-key dispatch (which is
;; what which-key's own built-in automatic popup relies on).
;;
;; These two kinds of keymap are fundamentally different: response
;; keymaps are consulted by ad-hoc Lisp code via `lookup-key' on a
;; local variable, one event at a time, invisible to
;; `this-single-command-keys' and everything which-key's automatic
;; mode polls.  There is no general way to discover which keymap is
;; in play at an arbitrary `read-key' call site -- see the Commentary
;; below for the two mechanisms this package uses instead.
;;
;; Usage:
;;
;;   (require 'mortal-response-keymap)
;;   (mortal-response-keymap-mode 1)
;;
;; After that, `query-replace', `query-replace-regexp',
;; `map-query-replace-regexp', `dired-do-query-replace-regexp',
;; `tags-query-replace', `xref-query-replace-in-results',
;; `project-query-replace-regexp', `y-or-n-p', `map-y-or-n-p', and
;; anything built on them (`save-some-buffers', `kill-some-buffers',
;; `revert-buffer' confirmations, and most third-party confirmation
;; prompts) will show a which-key popup automatically, with nothing
;; to register.
;;
;; This works by advising the small, fixed set of functions that
;; *own* a response keymap (`perform-replace', `y-or-n-p',
;; `map-y-or-n-p') rather than the unbounded set of commands that
;; might call them -- `y-or-n-p' in particular is called as a
;; subroutine from arbitrary callers, so `real-this-command'-based
;; registration cannot generalize to it; hooking the primitive itself
;; can.
;;
;; For a bespoke response keymap that bypasses all three primitives,
;; two escape hatches remain:
;;
;;   - `mortal-with-response-keymap', to wrap a `read-key' call you
;;     control:
;;
;;       (defvar-keymap my-map "y" #'ignore "n" #'ignore)
;;       (mortal-with-response-keymap my-map
;;         (read-key "Choose: "))
;;
;;   - `mortal-response-keymap-add' (M-x), to register a COMMAND ->
;;     KEYMAP fallback interactively, right after hitting a prompt
;;     that didn't show a popup.  With a prefix argument it persists
;;     the entry via Customize.
;;
;; Caveat: `which-key--show-keymap' and
;; `which-key--hide-popup-ignore-command' are which-key's private,
;; undocumented functions.  If a which-key update changes their
;; signature, this package will warn once via `display-warning' and
;; then silently stop showing popups for the rest of the session
;; rather than erroring repeatedly.

;;; Code:

(require 'which-key)

(defgroup mortal-response-keymap nil
  "Show which-key popups for response keymaps like `query-replace-map'."
  :group 'convenience)

(defcustom mortal-response-keymap-alist nil
  "Fallback alist mapping a command to the response keymap it reads with.
Each key is matched against both `real-this-command' and
`this-command' while a key is being read; the first match wins.  Each
value must be a symbol whose value is a keymap.

Only needed for bespoke response maps that do not go through
`perform-replace', `y-or-n-p', or `map-y-or-n-p' -- those three are
covered automatically by `mortal-response-keymap-mode' with no
registration required.  See `mortal-response-keymap-add' for an
interactive way to populate this."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'mortal-response-keymap)

(defcustom mortal-response-keymap-functions '(read-key)
  "Key-reading primitives that show the which-key popup when called.
`read-key' covers `query-replace-map' and the great majority of
response-map consumers.  Add `read-char', `read-char-choice', or
`read-event' here if you have code that reads its response with one
of those instead -- each addition is one more function this mode
intercepts globally, so only add what you actually need."
  :type '(repeat function)
  :set (lambda (sym val)
         (let ((was-on (and (boundp 'mortal-response-keymap-mode)
                             mortal-response-keymap-mode)))
           (when was-on (mortal-response-keymap-mode -1))
           (set-default sym val)
           (when was-on (mortal-response-keymap-mode 1))))
  :group 'mortal-response-keymap)

(defcustom mortal-response-keymap-title "Response keys"
  "Title shown by `which-key' above response keymaps."
  :type 'string
  :group 'mortal-response-keymap)

(defvar mortal-active-response-keymap nil
  "Dynamically bound to the response keymap currently in play.
Set automatically around `perform-replace', `y-or-n-p', and
`map-y-or-n-p' by `mortal-response-keymap-mode'.  Also usable
directly via `mortal-with-response-keymap' for ad-hoc prompts.")

(defvar mortal--last-command-for-registration nil
  "Command most recently seen reading a key; aids `mortal-response-keymap-add'.")

(defvar mortal--api-warned nil
  "Non-nil once the which-key API failure warning has fired this session.")

;;; Renderer: show/hide the popup around the actual key read.

(defun mortal--command-response-keymap ()
  "Return the keymap to show right now, or nil.
Checks the dynamic variable `mortal-active-response-keymap' first (set
by the setter advices below, or by `mortal-with-response-keymap'),
then falls back to `mortal-response-keymap-alist' for bespoke,
unhooked prompts."
  (or mortal-active-response-keymap
      (when-let* ((sym (or (alist-get real-this-command
                                       mortal-response-keymap-alist)
                            (alist-get this-command
                                        mortal-response-keymap-alist))))
        (and (boundp sym) (keymapp (symbol-value sym))
             (symbol-value sym)))))

(defun mortal--which-key-api-warn (fn err)
  "Warn once that FN failed against which-key's private API with ERR."
  (unless mortal--api-warned
    (setq mortal--api-warned t)
    (display-warning
     'mortal-response-keymap
     (format "which-key internal API `%s' failed (%s); \
mortal-response-keymap-mode will stop showing popups this session. \
This usually means which-key changed its private functions; \
please check for an update to mortal-response-keymap."
             fn (error-message-string err))
     :warning)))

(defun mortal--which-key-show (keymap)
  "Display KEYMAP in the which-key popup, if possible."
  (when (and which-key-mode (keymapp keymap)
             (fboundp 'which-key--show-keymap)
             (not mortal--api-warned))
    (condition-case err
        (which-key--show-keymap
         mortal-response-keymap-title keymap nil nil t)
      (error (mortal--which-key-api-warn 'which-key--show-keymap err)))))

(defun mortal--which-key-hide ()
  "Hide any popup shown by `mortal--which-key-show'."
  (when (and which-key-mode
             (fboundp 'which-key--hide-popup-ignore-command)
             (not mortal--api-warned))
    (condition-case err
        (which-key--hide-popup-ignore-command)
      (error (mortal--which-key-api-warn
              'which-key--hide-popup-ignore-command err)))))

(defun mortal--read-advice (orig-fn &rest args)
  "Around-advice for `mortal-response-keymap-functions': show/hide the popup."
  (setq mortal--last-command-for-registration real-this-command)
  (let ((keymap (mortal--command-response-keymap)))
    (unwind-protect
        (progn
          (when keymap (mortal--which-key-show keymap))
          (apply orig-fn args))
      (when keymap (mortal--which-key-hide)))))

;;; Setters: bind the dynamic var around the functions that own a keymap.

(defun mortal--perform-replace-advice (orig-fn &rest args)
  "Bind the active response keymap to `perform-replace's own MAP arg.
Covers `query-replace', `query-replace-regexp',
`map-query-replace-regexp', `dired-do-query-replace-regexp',
`tags-query-replace', `xref-query-replace-in-results', and
`project-query-replace-regexp' automatically, since they all
eventually call `perform-replace' -- no per-command registration
needed."
  (let ((mortal-active-response-keymap (or (nth 6 args) query-replace-map)))
    (apply orig-fn args)))

(defun mortal--y-or-n-p-advice (orig-fn &rest args)
  "Bind the active response keymap to `query-replace-map' for `y-or-n-p'.
Covers every caller of `y-or-n-p' throughout Emacs and third-party
packages -- e.g. `save-buffers-kill-emacs', `revert-buffer',
file-overwrite confirmations, and most packages' own confirmation
prompts -- without needing to know which command invoked it."
  (let ((mortal-active-response-keymap query-replace-map))
    (apply orig-fn args)))

(defun mortal--map-y-or-n-p-advice (orig-fn &rest args)
  "Bind the active response keymap for `map-y-or-n-p'.
Includes its own ACTION-ALIST argument when given (as
`save-some-buffers' and `kill-some-buffers' do), mirroring the merged
keymap `map-y-or-n-p' itself builds internally."
  (let* ((action-alist (nth 4 args))
         (mortal-active-response-keymap
          (if action-alist
              (append (mapcar (lambda (elt)
                                 (cons (car elt) (vector (nth 1 elt))))
                               action-alist)
                      query-replace-map)
            query-replace-map)))
    (apply orig-fn args)))

(defvar mortal--setter-advice-alist
  '((perform-replace . mortal--perform-replace-advice)
    (y-or-n-p        . mortal--y-or-n-p-advice)
    (map-y-or-n-p    . mortal--map-y-or-n-p-advice))
  "Alist of (FUNCTION . ADVICE) pairs that set the active response keymap.
This is the core mechanism: hooking the small set of functions that
own a response keymap, rather than the unbounded set of commands that
might call them.")

;;;###autoload
(define-minor-mode mortal-response-keymap-mode
  "Show a which-key popup for whichever response keymap is active.

Automatically covers `perform-replace', `y-or-n-p', and
`map-y-or-n-p' and everything built on them -- which is effectively
every commonly-used response keymap in vanilla Emacs, since nearly
all of them are built on these three.  `mortal-response-keymap-alist'
and `mortal-with-response-keymap' remain available for the rare
bespoke response map that bypasses all three.

If which-key's private API doesn't match what this mode expects (e.g.
after a which-key update), it warns once via `display-warning' and
then quietly stops trying for the rest of the session, rather than
either erroring on every keystroke or failing in total silence."
  :global t
  :group 'mortal-response-keymap
  (dolist (pair mortal--setter-advice-alist)
    (if mortal-response-keymap-mode
        (advice-add (car pair) :around (cdr pair))
      (advice-remove (car pair) (cdr pair))))
  (dolist (fn mortal-response-keymap-functions)
    (if mortal-response-keymap-mode
        (advice-add fn :around #'mortal--read-advice)
      (advice-remove fn #'mortal--read-advice))))

;;;###autoload
(defmacro mortal-with-response-keymap (keymap &rest body)
  "Mark KEYMAP as the active response keymap while BODY runs.
For ad-hoc prompts that don't go through `perform-replace',
`y-or-n-p', or `map-y-or-n-p'.  Requires `mortal-response-keymap-mode'
to be enabled to have any visible effect.

\(fn KEYMAP BODY...)"
  (declare (indent 1))
  `(let ((mortal-active-response-keymap ,keymap))
     ,@body))

;;;###autoload
(defun mortal-response-keymap-add (command keymap &optional save)
  "Register COMMAND as using response KEYMAP, as a fallback entry.
Only needed for bespoke prompts the setter advices don't already
cover; see `mortal-response-keymap-mode's docstring.

Interactively, COMMAND defaults to whatever command most recently
triggered a key read -- so the usual flow is: hit a prompt that
didn't show a popup, then immediately \\[mortal-response-keymap-add],
and just confirm or correct the command name and supply the keymap.

With a prefix argument, or non-nil SAVE when called from Lisp,
persist the change to `mortal-response-keymap-alist' via Customize so
it survives future sessions; otherwise the change only lasts this
session."
  (interactive
   (list (intern
          (completing-read
           "Command: " obarray #'commandp t
           (and mortal--last-command-for-registration
                (symbol-name mortal--last-command-for-registration))))
         (intern
          (completing-read
           "Response keymap variable: " obarray
           (lambda (s) (and (boundp s) (keymapp (symbol-value s))))
           t))
         current-prefix-arg))
  (setf (alist-get command mortal-response-keymap-alist) keymap)
  (if save
      (customize-save-variable 'mortal-response-keymap-alist
                                mortal-response-keymap-alist)
    (message "Registered %s -> %s for this session (C-u to persist)"
             command keymap)))

(provide 'mortal-response-keymap)

;;; mortal-response-keymap.el ends here
