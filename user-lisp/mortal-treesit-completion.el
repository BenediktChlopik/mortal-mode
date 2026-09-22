;;; -*- lexical-binding: t; -*-

;;; mortal-treesit-completion.el --- Tree-sitter completion for Mortal

(require 'treesit)
(require 'completion-preview)


;;; ---------------------------------------------------------------------------
;;; Background symbol cache
;;;
;;; Emacs Lisp has no real parallelism, so "async" here means: never do the
;;; expensive whole-buffer scan on the hot path (i.e. inside
;;; completion-at-point-functions, which is called synchronously while you
;;; type). Instead we maintain a buffer-local cache of symbols that is
;;; rebuilt in the background on an idle timer, using `while-no-input' so
;;; the rebuild bails out immediately if you start typing again. The actual
;;; completion function just reads the cache, which is effectively instant
;;; no matter how big the buffer is.
;;; ---------------------------------------------------------------------------

(defvar mortal-treesit-completion-idle-delay 0.3
  "Idle seconds to wait before (re)building the symbol cache.")

(defvar-local mortal--treesit-symbols nil
  "Cached list of symbols for the current buffer.")

(defvar-local mortal--treesit-symbols-timer nil
  "Idle timer used to rebuild `mortal--treesit-symbols'.")

(defvar-local mortal--treesit-symbols-dirty t
  "Non-nil when the cache needs rebuilding.")

(defun mortal--treesit-collect-symbols-1 ()
  "Do the actual buffer scan. May be aborted partway by `while-no-input'."
  (let (symbols)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\_<[[:word:]_]+\\_>" nil t)
        (push (match-string-no-properties 0) symbols)))
    (delete-dups symbols)))

(defun mortal--treesit-rebuild-cache (buffer)
  "Rebuild the symbol cache for BUFFER, interruptibly.
If interrupted by user input, reschedule instead of updating the cache."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (treesit-parser-list)
                 mortal--treesit-symbols-dirty)
        (let ((result (while-no-input
                        ;; Wrap in a cons so we can tell "aborted" (t)
                        ;; apart from a body that legitimately returns t.
                        (cons 'done (mortal--treesit-collect-symbols-1)))))
          (if (consp result)
              (progn
                (setq mortal--treesit-symbols (cdr result)
                      mortal--treesit-symbols-dirty nil))
            ;; Aborted (result is t) or buffer had no parser (nil): retry soon.
            (mortal--treesit-schedule-rebuild buffer)))))))

(defun mortal--treesit-schedule-rebuild (buffer)
  "(Re)schedule a debounced cache rebuild for BUFFER."
  (with-current-buffer buffer
    (when (timerp mortal--treesit-symbols-timer)
      (cancel-timer mortal--treesit-symbols-timer))
    (setq mortal--treesit-symbols-dirty t
          mortal--treesit-symbols-timer
          (run-with-idle-timer
           mortal-treesit-completion-idle-delay
           nil
           #'mortal--treesit-rebuild-cache
           buffer))))

(defun mortal--treesit-after-change (&rest _)
  "Mark the symbol cache dirty and schedule a rebuild."
  (mortal--treesit-schedule-rebuild (current-buffer)))


;;; ---------------------------------------------------------------------------
;;; Tree-sitter completion
;;; ---------------------------------------------------------------------------

(defun mortal/treesit-completion ()
  "Complete symbols found in the current Tree-sitter buffer."
  (when (treesit-parser-list)
    (let ((beg (save-excursion
                 (skip-syntax-backward "w_")
                 (point)))
          (end (point)))
      (list
       beg
       end
       (completion-table-dynamic
        (lambda (_)
          ;; Instant: just read the cache. Kick off a background rebuild
          ;; if we don't have one yet (e.g. first completion in buffer).
          (unless mortal--treesit-symbols
            (mortal--treesit-schedule-rebuild (current-buffer)))
          mortal--treesit-symbols)
        ;; Don't switch to the completions buffer while computing.
        t)))))


(defun mortal/treesit-completion-setup ()
  "Enable Tree-sitter completion in the current buffer."
  (when (treesit-parser-list)
    (add-hook 'completion-at-point-functions
              #'mortal/treesit-completion
              nil
              t)
    (add-hook 'after-change-functions
              #'mortal--treesit-after-change
              nil
              t)
    (mortal--treesit-schedule-rebuild (current-buffer))
    (completion-preview-mode 1)))

(defun mortal/treesit-completion-teardown ()
  "Disable Tree-sitter completion and clean up timers in the current buffer."
  (remove-hook 'completion-at-point-functions #'mortal/treesit-completion t)
  (remove-hook 'after-change-functions #'mortal--treesit-after-change t)
  (when (timerp mortal--treesit-symbols-timer)
    (cancel-timer mortal--treesit-symbols-timer))
  (setq mortal--treesit-symbols-timer nil))

(add-hook 'prog-mode-hook #'mortal/treesit-completion-setup)
(add-hook 'kill-buffer-hook #'mortal/treesit-completion-teardown)


;;; ---------------------------------------------------------------------------
;;; End
;;; ---------------------------------------------------------------------------

(provide 'mortal-treesit-completion)
;;; mortal-treesit-completion.el ends here
