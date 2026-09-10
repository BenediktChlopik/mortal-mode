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
(setq initial-scratch-message nil)


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
;; (add-hook 'prog-mode-hook #'mortal-mode)


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
(setq treesit-auto-install-grammar 'always)

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (js-mode . js-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (json-mode . json-ts-mode)
        (css-mode . css-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        (java-mode . java-ts-mode)
        (rust-mode . rust-ts-mode)))


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

;; show emacs special buffers in a nice side bar
(setq display-buffer-alist
      `(("\\*.*\\*"
         (display-buffer-in-side-window)
         (side . right)
         (window-width . 0.25)
         (dedicated . t)
         (preserve-size . (t . nil))
         (body-function . ,(lambda (window)
                              (with-current-buffer (window-buffer window)
                                (tab-line-mode -1)))))))

;; editing modes
(delete-selection-mode 1)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; selection mode
(cua-selection-mode 1)

(with-eval-after-load 'cua-base
  (define-key cua-global-keymap (kbd "C-<return>") nil))
(show-paren-mode 1)

;; electric stuff
(electric-indent-mode 1)
(electric-quote-mode 1)
(electric-pair-mode 1)
(electric-layout-mode 1)
(electric-layout-mode 1)
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
                (let ((win (window-main-window)))
                  (if (window-live-p win)
                      (select-window win)
                    (other-window 1)))
                (find-file file))))



;; highlight occurences
(defface mortal-region-occurrence-face
  '((t :background "#2e2e2e" :foreground unspecified))
  "Light face used to highlight occurrences of the marked region.")

(defvar-local mortal/region-ovs nil
  "Overlays used to highlight occurrences of the marked region in this buffer.")

(defun mortal/region-clear-overlays (&optional buffer)
  "Delete highlight overlays in BUFFER (or current buffer)."
  (when (buffer-live-p (or buffer (current-buffer)))
    (with-current-buffer (or buffer (current-buffer))
      (mapc #'delete-overlay mortal/region-ovs)
      (setq mortal/region-ovs nil))))

(defun mortal/region-highlight-update ()
  "Highlight occurrences of the marked region in every visible window."
  ;; Clear old overlays everywhere first.
  (dolist (win (window-list))
    (mortal/region-clear-overlays (window-buffer win)))
  (when (use-region-p)
    (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
      (when (and (> (length text) 0)
                 (string-match-p "[^[:space:]\n]" text))
        (dolist (win (window-list))
          (with-current-buffer (window-buffer win)
            (save-excursion
              (goto-char (point-min))
              (while (search-forward text nil t)
                (push (make-overlay (match-beginning 0) (match-end 0)) mortal/region-ovs)
                (overlay-put (car mortal/region-ovs) 'face 'mortal-region-occurrence-face)))))))))

(define-minor-mode mortal-region-occurrence-mode
  "Highlight occurrences of the marked region in all windows; unhighlight when unmarked."
  :lighter " RegHi"
  :global t
  (if mortal-region-occurrence-mode
      (add-hook 'post-command-hook #'mortal/region-highlight-update)
    (remove-hook 'post-command-hook #'mortal/region-highlight-update)
    (dolist (buf (buffer-list))
      (mortal/region-clear-overlays buf))))

(mortal-region-occurrence-mode 1)



;; hide minor modes
(setq mode-line-collapse-minor-modes
      '(eldoc-mode
        flymake-mode
        visual-line-mode
        which-key-mode
        company-mode
        completion-preview-mode
        hs-minor-mode
        mortal-region-occurrence-mode))


;; folding
(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq hs-show-indicators t)
(setq hs-indicator-type 'fringe)


;; eglot
(use-package eglot
  :ensure t)
(assoc major-mode eglot-server-programs)

;; flymake
(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'eglot-ensure)


;; right click context of all possible commands at point
(context-menu-mode 1)

;; focus frames
(setq focus-follows-mouse t)
(setq mouse-autoselect-window t)
