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

;; mortal mode
(require 'mortal-keymap)

(define-minor-mode mortal-mode
  "A minor mode for mortal-related keybindings."
  :lighter " Mortal"
  :keymap mortal-map
  :global t
  )

(add-to-list 'emulation-mode-map-alists
             `((mortal-mode . ,mortal-map)))

(mortal-mode 1)
;;(add-hook 'prog-mode-hook #'mortal-mode)


;; theming
(load-theme 'wombat t)

(custom-set-faces
 '(tab-line ((t (:inherit header-line))))
 '(tab-line-tab-current ((t (:inherit header-line :weight bold :underline t))))
 '(tab-line-tab-inactive ((t (:inherit header-line-inactive)))))


;; completions
(icomplete-mode 1)
(setq completion-auto-help 'always)
(setq completion-preview-minimum-symbol-length 1)
(global-completion-preview-mode 1)

(require 'treesit)

(defun mortal/treesit-completion ()
  "Complete symbols found in the current Tree-sitter buffer."
  (when (treesit-parser-list)
    (let ((beg (save-excursion
                 (skip-syntax-backward "w_")
                 (point)))
          (end (point)))
      (list beg end
            (completion-table-dynamic
             (lambda (_)
               (let (symbols)
                 (save-excursion
                   (goto-char (point-min))
                   (while (re-search-forward
                           "\\_<[[:word:]_]+\\_>" nil t)
                     (push (match-string-no-properties 0) symbols)))
                 (delete-dups symbols))))))))


(defun mortal/treesit-completion-setup ()
  "Enable Tree-sitter completion without changing any keybindings."
  (when (treesit-parser-list)
    (add-hook 'completion-at-point-functions
              #'mortal/treesit-completion
              nil t)
    (completion-preview-mode 1)))

(add-hook 'prog-mode-hook #'mortal/treesit-completion-setup)

;; treesitter
(setq treesit-enabled-modes t)
(setq treesit-auto-install-grammar 'always)


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

;; selection mode
(cua-selection-mode 1)

(with-eval-after-load 'cua-base
  (define-key cua-global-keymap (kbd "C-<return>") nil))
(show-paren-mode 1)
(setq initial-scratch-message nil)

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
  (setq speedbar-prefer-window t
        speedbar-window-default-width 25
        speedbar-window-max-width 25)

  ;; Emacs 31.1 mouse fix
  (defun mortal/speedbar-fix (&rest _)
    (setq speedbar-buffer
          (or (and (buffer-live-p speedbar-buffer) speedbar-buffer)
              (get-buffer-create speedbar--buffer-name)))
    (with-current-buffer speedbar-buffer
      (speedbar-mode)))
  (advice-add 'speedbar-window-mode :before #'mortal/speedbar-fix)

  ;; Open Speedbar files in the main window.
  (advice-add 'speedbar-find-file-in-frame :override
              (lambda (file)
                (select-window (window-main-window))
                (find-file file))))



;; hide minor modes
(setq mode-line-collapse-minor-modes
      '(eldoc-mode
        flymake-mode
        visual-line-mode
        which-key-mode
        company-mode
        completion-preview-mode
        hs-minor-mode))


;; folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq hs-show-indicators t)
(setq hs-indicator-type 'fringe)


;; eglot
(require 'eglot)
(assoc major-mode eglot-server-programs)

;; flymake
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'eglot-ensure)

;; term
(add-hook 'term-exec-hook
          (lambda ()
            (tab-line-mode -1)
            (set-process-query-on-exit-flag
             (get-buffer-process (current-buffer))
             nil)))
