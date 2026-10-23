;; -*- lexical-binding: t; -*-

(let ((dir (expand-file-name "user-lisp/" user-emacs-directory)))
  (when (file-directory-p dir)
    (add-to-list 'load-path dir)
    (dolist (file (directory-files dir t "\\.el\\'"))
      (load file nil 'nomessage))))


(use-package mortal-mode
  :load-path "~/.emacs.d/mortal-mode"
  :config
  (mortal-mode 1))

(setq mortal-move-style 'gecko)


(add-to-list 'load-path "~/.emacs.d/mortal-mode/base-config")

(require 'mortal-base-config)

;; Add keys like this:
;; (require 'mortal-keymap)
;; (mortal/add-keys
;;  "C-=" #'foo
;;  "M-/" #'bar
;;  "<f5>" my-prefix-map)
