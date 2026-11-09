;; -*- lexical-binding: t; -*-

;;; Persistent Emacs crash/debug logging

(defvar mortal/debug-log-file
  (expand-file-name "emacs-debug.log" user-emacs-directory))

(defvar mortal/previous-debug-log-file
  (expand-file-name "emacs-debug.previous.log" user-emacs-directory))


;;; Save backtrace whenever the debugger is invoked

(defun mortal/save-debugger-backtrace (&rest args)
  "Save the current Lisp backtrace to disk, then enter the debugger."
  (condition-case err
      (let ((inhibit-debugger t))
        (with-temp-buffer
          (insert
           (format "\n\n===== %s =====\nDebugger arguments: %S\n\n"
                   (format-time-string "%Y-%m-%d %H:%M:%S %z")
                   args))
          (let ((standard-output (current-buffer)))
            (backtrace))
          (write-region
           (point-min) (point-max)
           mortal/debug-log-file
           t 'silent)
          (set-file-modes mortal/debug-log-file #o600)))
    (error
     (message "mortal: Failed to save backtrace: %S" err)))

  ;; Continue with the normal Emacs debugger.
  (apply #'debug args))


;;; Enable debugging

(setq debugger #'mortal/save-debugger-backtrace
      debug-on-error t
      debug-on-quit t)


;;; Restore previous crash log on startup

(defun mortal/open-previous-debug-log ()
  "Rotate the last session's debug log and open it."
  (when (file-exists-p mortal/debug-log-file)
    (rename-file mortal/debug-log-file
                 mortal/previous-debug-log-file
                 t))

  (when (file-exists-p mortal/previous-debug-log-file)
    (find-file mortal/previous-debug-log-file)))

(add-hook 'emacs-startup-hook #'mortal/open-previous-debug-log)


;;; mortal mode

(add-to-list 'load-path
             (expand-file-name "mortal-mode/" user-emacs-directory))

(use-package mortal-mode
  :config
  (mortal-mode 1))

(setq mortal-move-style 'gecko)

;; Add keys like this:
;; (require 'mortal-keymap)
;; (mortal/add-keys
;;  "C-=" #'foo
;;  "M-/" #'bar
;;  "<f5>" my-prefix-map)


(add-to-list 'load-path 
             (expand-file-name "mortal-mode/base-config" user-emacs-directory))
(require 'mortal-base-config)


;; Load everything in user-lisp/. Errors are deliberately allowed to
;; escape, so a broken file crashes startup rather than being ignored.
(add-hook 'after-init-hook
          (lambda ()
            (let ((dir (expand-file-name "user-lisp/" user-emacs-directory)))
              (when (file-directory-p dir)
                (add-to-list 'load-path dir)
                (byte-recompile-directory dir 0)
                (dolist (file (directory-files dir t "\\.el\\'"))
                  (load file nil 'nomessage)))))
          -90)


;; Trust your own Emacs Lisp source directory.
(with-eval-after-load 'files
  (add-to-list 'trusted-content
               (abbreviate-file-name
                (file-name-as-directory
                 (expand-file-name "~/.emacs.d/user-lisp/")))))
 

;; Trust your own Emacs Lisp directory (Emacs 30+).
(when (boundp 'trusted-files)
  (add-to-list 'trusted-files
               "/home/raptus/.emacs.d/user-lisp/got/"))


