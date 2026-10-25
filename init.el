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


(add-hook 'after-init-hook
          (lambda ()
            (let ((dir (expand-file-name "user-lisp/" user-emacs-directory)))
              (add-to-list 'load-path dir)
              (byte-recompile-directory dir 0)
              (mapc (lambda (file) (load file nil 'nomessage))
                    (directory-files dir t "\\.el\\'")))))
