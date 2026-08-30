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
 '(tab-line ((t (:inherit header-line))))
 '(tab-line-tab-current ((t (:inherit header-line :weight bold :underline t))))
 '(tab-line-tab-inactive ((t (:inherit header-line-inactive)))))


;; imports
(require 'mortal-keymap)

(define-minor-mode mortal-mode
  "A minor mode for mortal-related keybindings."
  :lighter " Mortal"
  :keymap mortal-map
  :global t
  (cua-mode (if mortal-mode -1 1))
  )

(mortal-mode 1)
;;(add-hook 'prog-mode-hook #'mortal-mode)


;; cursor style
(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq-default cursor-type '(bar . 2))

;; highlight line
(global-hl-line-mode 1)
(set-face-attribute 'hl-line nil
                    :inherit nil
                    :background "#292929")


;; disable emacs standard ui
(menu-bar-mode -1)
(tool-bar-mode -1)

;; other ui modes
(scroll-bar-mode -1)
(which-key-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; tab line
(global-tab-line-mode 1)
(setq tab-line-close-button-show nil)

;; save sessions
(desktop-save-mode 1)
(save-place-mode 1)


;; editing modes
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(cua-selection-mode 1)
(add-hook 'prog-mode-hook #'delete-trailing-whitespace-mode)
(show-paren-mode 1)


;; electric stuff
(electric-quote-mode 1)
(electric-pair-mode 1)
(electric-layout-mode 1)
(electric-layout-mode 1)
(electric-indent-mode 1)
(setq electric-pair-pairs
      '((?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})
        (?\< . ?\>)))


;; completions
(icomplete-mode 1)
(setq completion-auto-help 'always)
(setq completion-preview-minimum-symbol-length 1)
(global-completion-preview-mode 1)


;; scrolling
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)


;; search
(setq isearch-wrap-pause 'no)
(setq isearch-wrap-pause nil)
(setq isearch-lazy-count t)
(setq isearch-allow-motion t)


;; speedbar
(with-eval-after-load 'speedbar
  (setq speedbar-window-default-width 30
        speedbar-window-max-width 30
        speedbar--window-width 30))
