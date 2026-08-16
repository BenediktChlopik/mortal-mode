(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(load "multiple-cursors")
(load "python-stuff")

(require 'mortal-keymap)


(define-minor-mode mortal-mode
  "A minor mode for mortal-related keybindings."
  :lighter " Mortal"
  :keymap mortal-map
)

(add-hook 'prog-mode-hook #'mortal-mode)

(tool-bar-mode -1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(delete-selection-mode 1)
(tab-bar-mode 1)

(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq-default cursor-type '(bar . 2))

(desktop-save-mode 1)

(setq-default make-backup-files nil)
(setq-default auto-save-default nil)
