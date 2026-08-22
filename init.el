;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((Lexical-binding . t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; nobody needs those
(setq-default make-backup-files nil)
(setq-default auto-save-default nil)

;; theming
(load-theme 'wombat t)

(custom-set-faces
 '(tab-bar ((t (:inherit header-line))))
 '(tab-bar-tab ((t (:inherit header-line :weight bold :underline t))))
 '(tab-bar-tab-inactive ((t (:inherit header-line-inactive)))))


;; imports
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'mortal-keymap)


(define-minor-mode mortal-mode
  "A minor mode for mortal-related keybindings."
  :lighter " Mortal"
  :keymap mortal-map
  :global t
  )

(mortal-mode)
;; (add-hook 'prog-mode-hook #'mortal-mode)


;; cursor style
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq-default cursor-type '(bar . 2))


;; ui modes
(tab-bar-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)


;; save sessions
(desktop-save-mode 1)


;; editing modes
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-local python-indent-offset 4)

(add-hook 'python-ts-mode-hook
          (lambda ()
            (setq-local python-indent-offset 4)
            (setq-local indent-tabs-mode nil)
            (setq-local tab-width 4)
            (add-hook 'before-save-hook
                      #'delete-trailing-whitespace nil t)))

(add-to-list 'major-mode-remap-alist
             '(python-mode . python-ts-mode))
