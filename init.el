;; -*- lexical-binding: t; -*-

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


;; Load everything in user-lisp/.  One broken file must not stop the rest of
;; startup: an error escaping `after-init-hook' skips the hooks after it,
;; including `desktop-read', which would silently lose all restored buffers
;; and with them the window layout.  Depth -90 runs this before Desktop
;; restores, so modes defined here can restore their own buffers.
(add-hook 'after-init-hook
          (lambda ()
            (let ((dir (expand-file-name "user-lisp/" user-emacs-directory)))
              (when (file-directory-p dir)
                (add-to-list 'load-path dir)
                (condition-case err
                    (byte-recompile-directory dir 0)
                  (error (message "user-lisp: byte-compile failed: %S" err)))
                (dolist (file (directory-files dir t "\\.el\\'"))
                  (condition-case err
                      (load file nil 'nomessage)
                    (error
                     (display-warning
                      'user-lisp
                      (format "Error loading %s: %s" (file-name-nondirectory file)
                              (error-message-string err))
                      :error)))))))
          -90)
