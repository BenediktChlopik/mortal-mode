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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(deadgrep graph-fa2 macher-agent markdown-mode mcp vterm))
 '(package-vc-selected-packages
   '((macher-agent :url "https://github.com/elij/macher-agent/"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:inherit nil :background "#292929"))))
 '(tab-line ((t (:inherit header-line))))
 '(tab-line-tab-current ((t (:inherit header-line :weight bold :underline t))))
 '(tab-line-tab-inactive ((t (:inherit header-line-inactive)))))
