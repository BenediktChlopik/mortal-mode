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


(require 'tab-line)

(eval-when-compile
  (require 'speedbar))

(declare-function speedbar-mode "speedbar")
(declare-function speedbar-window-mode "speedbar")
(declare-function speedbar-find-file-in-frame "speedbar")

(defvar completion-preview-minimum-symbol-length)
(defvar hs-show-indicators)
(defvar hs-indicator-type)

;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun mortal-kill-undisplayed-buffers ()
  "Kill buffers not displayed or listed in any tab line."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless
        (or (get-buffer-window buffer t)
            (string-match-p "\\`[ *]" (buffer-name buffer))
            (let ((p (get-buffer-process buffer)))
              (and p (process-live-p p)))
            (catch 'tab-line
              (walk-windows
               (lambda (w)
                 (when (memq buffer
                             (window-parameter w 'tab-line-buffers))
                   (throw 'tab-line t)))
               nil t))
      (kill-buffer buffer)))))

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

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

;;; Cursor

(blink-cursor-mode 1)
(setq blink-cursor-blinks 0)
(setq-default cursor-type '(bar . 2))


;;; Highlight current line

(global-hl-line-mode 1)


;;; write custom vars in a different file

(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file 'noerror)

;;; Line numbers

(add-hook 'prog-mode-hook #'display-line-numbers-mode)


;;; Tab line

(setq tab-line-close-button-show nil)
(require 'tab-line-drag)

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

(setq completion-styles '(basic flex))

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

;;; ---------------------------------------------------------------------------
;;; Desktop
;;; ---------------------------------------------------------------------------

(save-place-mode 1)
(savehist-mode 1)

(setq desktop-dirname user-emacs-directory
      desktop-path (list user-emacs-directory)
      desktop-save t
      desktop-restore-frames nil        ; the layout code below handles frames
      desktop-restore-eager t
      desktop-auto-save-timeout 30
      desktop-load-locked-desktop t)
(desktop-save-mode 1)

;;; ---------------------------------------------------------------------------
;;; Layout persistence (frame, windows, side windows, tabs)
;;; ---------------------------------------------------------------------------

(defvar mortal/layout-file (expand-file-name "layout.eld" user-emacs-directory))

(defvar mortal/layout-ready nil
  "Non-nil once the layout has been restored.  Saving is skipped before that,
so a failed restore can't overwrite the last good layout.")

(defconst mortal/frame-params '(left top width height fullscreen))

(add-to-list 'window-persistent-parameters '(mortal-tabs . writable))

(defmacro mortal/logged (&rest body)
  "Run BODY; report errors in *Messages* instead of hiding them."
  `(condition-case err (progn ,@body)
     (error (message "mortal layout: %S" err))))

;; Buffers can't be written to disk, so they are stored as file names/buffer names.
(defun mortal/buffer-key (buf)
  (or (buffer-file-name buf) (buffer-name buf)))

(defun mortal/key-buffer (key)
  (or (find-buffer-visiting key) (get-buffer key)))

(defun mortal/speedbar-open-p ()
  (and (boundp 'speedbar-buffer)
       (buffer-live-p speedbar-buffer)
       (get-buffer-window speedbar-buffer)
       t))

(defun mortal/window-tab-keys (window)
  "Keys of the buffers shown in WINDOW's tab line."
  (with-selected-window window
    (delq nil (mapcar (lambda (b) (and (buffer-live-p b) (mortal/buffer-key b)))
                      (and (bound-and-true-p tab-line-tabs-function)
                           (funcall tab-line-tabs-function))))))

(defun mortal/side-window-specs ()
  "Describe every side window except Speedbar's."
  (let (specs)
    (dolist (w (window-list nil 'no-mini) specs)
      (when-let* ((side (window-parameter w 'window-side)))
        (unless (and (boundp 'speedbar-buffer) (eq (window-buffer w) speedbar-buffer))
          (push (list :side side
                      :slot (or (window-parameter w 'window-slot) 0)
                      :size (if (memq side '(left right))
                                (window-total-width w)
                              (window-total-height w))
                      :key (mortal/buffer-key (window-buffer w)))
                specs))))))

(defun mortal/layout-save ()
  (when mortal/layout-ready
    (mortal/logged
     (walk-windows (lambda (w)
                     (set-window-parameter w 'mortal-tabs (mortal/window-tab-keys w)))
                   'no-mini)
     (with-temp-file mortal/layout-file
       (let ((print-length nil) (print-level nil))
         (prin1 (list :frame (seq-filter (lambda (p) (memq (car p) mortal/frame-params))
                                         (frame-parameters))
                      :speedbar (mortal/speedbar-open-p)
                      :sides (mortal/side-window-specs)
                      :main (window-state-get (window-main-window) t))
                (current-buffer)))))))

(defun mortal/restore-side-window (spec)
  (when-let* ((buf (mortal/key-buffer (plist-get spec :key))))
    (let ((side (plist-get spec :side))
          (size (plist-get spec :size)))
      (display-buffer-in-side-window
       buf `((side . ,side)
             (slot . ,(plist-get spec :slot))
             (,(if (memq side '(left right)) 'window-width 'window-height) . ,size))))))

(defun mortal/restore-tabs ()
  (walk-windows
   (lambda (w)
     (when-let* ((keys (window-parameter w 'mortal-tabs)))
       (set-window-parameter
        w 'tab-line-buffers (delq nil (mapcar #'mortal/key-buffer keys)))))
   'no-mini))

(defun mortal/layout-restore ()
  (message "mortal layout: restoring (windows now: %d, file exists: %s)"
           (length (window-list nil 'no-mini))
           (file-exists-p mortal/layout-file))
  (mortal/logged
   (when (file-exists-p mortal/layout-file)
     (let ((data (with-temp-buffer
                   (insert-file-contents mortal/layout-file)
                   (read (current-buffer)))))
       (modify-frame-parameters nil (plist-get data :frame))
       (window-state-put (plist-get data :main) (window-main-window) 'safe)
       (mapc #'mortal/restore-side-window (plist-get data :sides))
       (mortal/restore-tabs)
       (when (plist-get data :speedbar) (speedbar-window-mode 1))
       (force-mode-line-update t))))
  (message "mortal layout: restore done (windows now: %d)"
           (length (window-list nil 'no-mini)))
  (setq mortal/layout-ready t))

(add-hook 'kill-emacs-hook #'mortal/layout-save)
(add-hook 'window-setup-hook #'mortal/layout-restore)
(run-with-idle-timer 60 t #'mortal/layout-save)

;;; ---------------------------------------------------------------------------
;;; gptel chats: back each chat buffer with a file in ~/gptel-chats/
;;; ---------------------------------------------------------------------------

(defconst mortal/gptel-dir (expand-file-name "~/gptel-chats/"))

(defun mortal/gptel-attach-file ()
  (unless buffer-file-name
    (make-directory mortal/gptel-dir t)
    (let ((dir default-directory))
      (set-visited-file-name
       (expand-file-name (format-time-string "chat-%F-%H%M%S.md") mortal/gptel-dir))
      (setq default-directory dir)
      (add-file-local-variable 'default-directory dir)
      (save-buffer))))
(add-hook 'gptel-mode-hook #'mortal/gptel-attach-file)

(setq auto-save-visited-interval 5
      auto-save-visited-predicate (lambda () (bound-and-true-p gptel-mode)))
(auto-save-visited-mode 1)

;;; ---------------------------------------------------------------------------
;;; Eglot / Flymake
;;; ---------------------------------------------------------------------------

(use-package eglot
  :ensure t)

(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'prog-mode-hook #'eglot-ensure)


;;; ---------------------------------------------------------------------------
;;; Context menu
;;; ---------------------------------------------------------------------------

(context-menu-mode 1)

;; -------------------------------------------------------------------------
;; Which key
;; -------------------------------------------------------------------------

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


;; -------------------------------------------------------------------------
;; window selection
;; -------------------------------------------------------------------------

(setq mouse-autoselect-window 0.2)


;;; ---------------------------------------------------------------------------
;;; End
;;; ---------------------------------------------------------------------------

(provide 'mortal-base-config)
;;; mortal-base-config.el ends here
