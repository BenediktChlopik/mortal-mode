;; -*- lexical-binding: t; -*-

(require 'mortal-base-config)


(setq mortal-move-style 'smart)

(require 'mortal-mode)

(mortal-mode 1)


(add-to-list 'load-path
             (expand-file-name "your-config-files/"
                               user-lisp-directory))

(require 'alternative-keybinds)
(require 'your-config-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(@ gptel)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit nil :background "#292929"))))
 '(tab-line ((t (:inherit header-line))))
 '(tab-line-tab-current ((t (:inherit header-line :weight bold :underline t))))
 '(tab-line-tab-inactive ((t (:inherit header-line-inactive)))))
