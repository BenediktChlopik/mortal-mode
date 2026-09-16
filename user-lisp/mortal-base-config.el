;;; -*- lexical-binding: t; -*-

;;; mortal-base-config.el --- Base Emacs configuration

;;; ---------------------------------------------------------------------------
;;; Dependencies
;;; ---------------------------------------------------------------------------

(require 'which-key)
(require 'treesit)
(require 'use-package)
(require 'flymake)

(require 'cua-base)
(require 'elec-pair)
(require 'hideshow)
(require 'completion-preview)

(eval-when-compile
  (require 'speedbar))

(declare-function speedbar-mode "speedbar")
(declare-function speedbar-window-mode "speedbar")
(declare-function speedbar-find-file-in-frame "speedbar")

(defvar completion-preview-minimum-symbol-length)
(defvar hs-show-indicators)
(defvar hs-indicator-type)


;;; ---------------------------------------------------------------------------
;;; General
;;; ---------------------------------------------------------------------------

(setq-default make-backup-files nil
              auto-save-default nil)

(setq initial-scratch-message nil)

(winner-mode 1)


;;; ---------------------------------------------------------------------------
;;; Theme / appearance
;;; ---------------------------------------------------------------------------

(load-theme 'wombat t)

(custom-set-faces
 '(tab-line
   ((t (:inherit header-line))))
 '(tab-line-tab-current
   ((t (:inherit header-line
                 :weight bold
                 :underline t))))
 '(tab-line-tab-inactive
   ((t (:inherit header-line-inactive))))
 '(hl-line
   ((t (:inherit nil
                 :background "#292929")))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;; Cursor

(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq-default cursor-type '(bar . 2))


;;; Highlight current line

(global-hl-line-mode 1)


;;; Line numbers

(add-hook 'prog-mode-hook #'display-line-numbers-mode)


;;; Tab line

(setq tab-line-close-button-show nil)


;;; Hide minor modes

(setq mode-line-collapse-minor-modes
      '(eldoc-mode
        flymake-mode
        visual-line-mode
        which-key-mode
        company-mode
        completion-preview-mode
        hs-minor-mode))


;;; ---------------------------------------------------------------------------
;;; Completion
;;; ---------------------------------------------------------------------------

(icomplete-mode 1)

(setq completion-auto-help 'always
      completion-preview-minimum-symbol-length 1)

(global-completion-preview-mode 1)


;;; ---------------------------------------------------------------------------
;;; Tree-sitter
;;; ---------------------------------------------------------------------------

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


(require 'mortal-treesit-completion)

;;; ---------------------------------------------------------------------------
;;; Editing
;;; ---------------------------------------------------------------------------

(setq-default indent-tabs-mode nil
              tab-width 4)

(show-paren-mode 1)


;;; Folding

(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq hs-show-indicators t
      hs-indicator-type 'fringe)

(define-key flymake-mode-map [left-fringe mouse-1] nil)


;;; ---------------------------------------------------------------------------
;;; Scrolling
;;; ---------------------------------------------------------------------------

(setq scroll-margin 3
      scroll-conservatively 101
      scroll-preserve-screen-position t)


;;; ---------------------------------------------------------------------------
;;; Search
;;; ---------------------------------------------------------------------------

(setq isearch-wrap-pause nil
      isearch-lazy-count t
      isearch-allow-motion t)


;;; ---------------------------------------------------------------------------
;;; Sessions
;;; ---------------------------------------------------------------------------

(desktop-save-mode 1)
(save-place-mode 1)


;;; ---------------------------------------------------------------------------
;;; Special buffers
;;; ---------------------------------------------------------------------------

(setq display-buffer-alist
      `(("\\*.*\\*"
         (display-buffer-in-side-window)
         (side . right)
         (window-width . 0.25)
         (dedicated . t)
         (preserve-size . (t . nil))
         (body-function
          . ,(lambda (window)
               (with-current-buffer (window-buffer window)
                 (tab-line-mode -1)))))))


(defun mortal/force-same-window (buffer _alist)
  "Force BUFFER into the selected window, overriding
inhibit-same-window/dedication."
  (unless (window-minibuffer-p)
    (when (window-dedicated-p)
      (set-window-dedicated-p (selected-window) nil))
    (set-window-buffer (selected-window) buffer)
    (selected-window)))

(setq display-buffer-alist
      (append
       display-buffer-alist
       '(((category . xref-jump)
          (display-buffer-reuse-window
           display-buffer-same-window
           display-buffer-pop-up-window)))
       '((".*"
          (display-buffer-reuse-window mortal/force-same-window)))))

(setq switch-to-buffer-obey-display-actions t)


;;; ---------------------------------------------------------------------------
;;; Eglot / Flymake
;;; ---------------------------------------------------------------------------

(use-package eglot
  :ensure t)

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'eglot-ensure)


;;; ---------------------------------------------------------------------------
;;; Context menu / mouse focus
;;; ---------------------------------------------------------------------------

(context-menu-mode 1)

(setq focus-follows-mouse t
      mouse-autoselect-window t)


;;; ---------------------------------------------------------------------------
;;; End
;;; ---------------------------------------------------------------------------

(provide 'mortal-base-config)
;;; mortal-base-config.el ends here
