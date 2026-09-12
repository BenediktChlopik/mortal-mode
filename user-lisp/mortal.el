;;; -*- lexical-binding: t; -*-

;;; mortal.el --- Main Emacs configuration


;;; ---------------------------------------------------------------------------
;;; Dependencies
;;; ---------------------------------------------------------------------------

(require 'mortal-keymap)
(require 'which-key)
(require 'treesit)
(require 'use-package)

(require 'cua-base)
(require 'elec-pair)
(require 'hideshow)
(require 'completion-preview)

(eval-when-compile
  (require 'speedbar))

(declare-function mortal-response-keymap-mode "mortal-keymap")
(declare-function speedbar-mode "speedbar")
(declare-function speedbar-window-mode "speedbar")
(declare-function speedbar-find-file-in-frame "speedbar")

(defvar completion-preview-minimum-symbol-length)
(defvar electric-pair-pairs)
(defvar hs-show-indicators)
(defvar hs-indicator-type)


;;; ---------------------------------------------------------------------------
;;; General
;;; ---------------------------------------------------------------------------

(setq-default make-backup-files nil
              auto-save-default nil)

(setq initial-scratch-message nil)


;;; ---------------------------------------------------------------------------
;;; Mortal mode
;;; ---------------------------------------------------------------------------

(defgroup mortal nil
  "Mortal Emacs configuration."
  :group 'emacs)

(define-minor-mode mortal-mode
  "A minor mode for mortal-related keybindings."
  :lighter " Mortal"
  :keymap mortal-map
  :global t
  :group 'mortal)

(add-to-list 'emulation-mode-map-alists
             `((mortal-mode . ,mortal-map)))

(mortal-mode 1)

(mortal-response-keymap-mode 1)


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

(global-tab-line-mode 1)
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


(defun mortal/treesit-completion ()
  "Complete symbols found in the current Tree-sitter buffer."
  (when (treesit-parser-list)
    (let ((beg (save-excursion
                 (skip-syntax-backward "w_")
                 (point)))
          (end (point)))
      (list
       beg
       end
       (completion-table-dynamic
        (lambda (_)
          (let (symbols)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward
                      "\\_<[[:word:]_]+\\_>"
                      nil
                      t)
                (push (match-string-no-properties 0)
                      symbols)))
            (delete-dups symbols))))))))


(defun mortal/treesit-completion-setup ()
  "Enable Tree-sitter completion in the current buffer."
  (when (treesit-parser-list)
    (add-hook 'completion-at-point-functions
              #'mortal/treesit-completion
              nil
              t)
    (completion-preview-mode 1)))


(add-hook 'prog-mode-hook #'mortal/treesit-completion-setup)


;;; ---------------------------------------------------------------------------
;;; Editing
;;; ---------------------------------------------------------------------------

(delete-selection-mode 1)

(setq-default indent-tabs-mode nil
              tab-width 4)

(show-paren-mode 1)

(define-key cua-global-keymap
            (kbd "C-<return>")
            nil)


;;; Electric editing

(electric-indent-mode 1)
(electric-quote-mode 1)
(electric-pair-mode 1)
(electric-layout-mode 1)

(setq electric-pair-pairs
      '((?\( . ?\))
        (?\[ . ?\])
        (?\{ . ?\})
        (?\< . ?\>)))


;;; Folding

(add-hook 'prog-mode-hook #'hs-minor-mode)

(setq hs-show-indicators t
      hs-indicator-type 'fringe)


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
;;; Which-key
;;; ---------------------------------------------------------------------------

(which-key-mode 1)

(dolist (entry (accessible-keymaps global-map))
  (let ((keymap (cdr entry)))
    (unless (lookup-key keymap (kbd "<next>"))
      (define-key keymap
                  (kbd "<next>")
                  #'which-key-show-next-page-cycle))
    (unless (lookup-key keymap (kbd "<prior>"))
      (define-key keymap
                  (kbd "<prior>")
                  #'which-key-show-previous-page-cycle))))


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


;;; ---------------------------------------------------------------------------
;;; Eglot / Flymake
;;; ---------------------------------------------------------------------------

(use-package eglot
  :ensure t)

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'eglot-ensure)


;;; ---------------------------------------------------------------------------
;;; Speedbar
;;; ---------------------------------------------------------------------------

(defun mortal/speedbar-fix (&rest _)
  "Ensure Speedbar has a live buffer."
  (setq speedbar-buffer
        (or (and (boundp 'speedbar-buffer)
                 (buffer-live-p speedbar-buffer)
                 speedbar-buffer)
            (get-buffer-create speedbar--buffer-name)))

  (with-current-buffer speedbar-buffer
    (speedbar-mode)))


(with-eval-after-load 'speedbar

  (setq speedbar-prefer-window t
        speedbar-window-default-width 25
        speedbar-window-max-width 25)

  ;; Emacs 31.1 mouse fix.
  (advice-add #'speedbar-window-mode
              :before
              #'mortal/speedbar-fix)

  ;; Open Speedbar files in the main window.
  (advice-add #'speedbar-find-file-in-frame
              :override
              (lambda (file)
                (let ((win (window-main-window)))
                  (if (window-live-p win)
                      (select-window win)
                    (other-window 1)))
                (find-file file))))


;;; ---------------------------------------------------------------------------
;;; Context menu / mouse focus
;;; ---------------------------------------------------------------------------

(context-menu-mode 1)

(setq focus-follows-mouse t
      mouse-autoselect-window t)


;;; ---------------------------------------------------------------------------
;;; End
;;; ---------------------------------------------------------------------------

(provide 'mortal)
;;; mortal.el ends here
