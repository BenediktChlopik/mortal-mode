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
;;
;; The layout is written to `mortal/layout-file' shortly after every change
;; (and on exit), not only when Emacs quits cleanly, so a crash, a kill or a
;; logout doesn't lose it.  On startup it is restored after Desktop has
;; brought the file buffers back.  Buffers are matched by file name, not by
;; buffer name, and are re-opened if Desktop didn't restore them.

(defvar mortal/layout-file (expand-file-name "layout.eld" user-emacs-directory))

(defvar mortal/layout-ready nil
  "Non-nil once the layout has been restored (or there was nothing to restore).
Saving is skipped before that, so startup can't overwrite the last layout.")

(defvar mortal/layout-restoring nil
  "Non-nil while the layout is being restored; saving is suspended.")

(defvar mortal/layout-save-timer nil)

(defvar mortal/layout-normal-frame nil
  "Last left/top/width/height seen while the frame was not maximized or
fullscreen.  A maximized frame reports its maximized size, which is useless
for getting back to the un-maximized geometry.")

(defconst mortal/frame-geometry-params '(left top width height))

(add-to-list 'window-persistent-parameters '(mortal-tabs . writable))

(defmacro mortal/logged (&rest body)
  "Run BODY; report errors in *Messages* instead of hiding them.
Return non-nil if BODY ran without error."
  `(condition-case err (progn ,@body t)
     (error (message "mortal layout: %S" err) nil)))

;; Buffers can't be written to disk, so they are stored as file names/buffer names.
(defun mortal/buffer-key (buf)
  (or (buffer-file-name buf) (buffer-name buf)))

(defun mortal/key-buffer (key &optional open)
  "Return the live buffer for KEY (a file name or a buffer name).
With OPEN, visit KEY if it is a readable local file that isn't open yet."
  (or (find-buffer-visiting key)
      (get-buffer key)
      (and open
           (stringp key)
           (file-name-absolute-p key)
           (not (file-remote-p key))
           (file-readable-p key)
           (not (file-directory-p key))
           (find-file-noselect key))))

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

(defun mortal/file-buffer-names ()
  "Alist (BUFFER-NAME . FILE) of all file buffers.
`window-state-get' only records buffer names; this lets the restore find the
right buffer even when the names came out differently."
  (delq nil (mapcar (lambda (b)
                      (when-let* ((f (buffer-file-name b)))
                        (cons (buffer-name b) f)))
                    (buffer-list))))

;;;; Frame

(defun mortal/frame-usable-p (&optional frame)
  "Non-nil if FRAME is a normal graphical frame whose geometry we manage."
  (let ((f (or frame (selected-frame))))
    (and (frame-live-p f)
         (display-graphic-p f)
         (not (frame-parameter f 'parent-frame)))))

(defun mortal/frame-note-geometry (&rest _)
  "Remember the selected frame's geometry while it is in its normal state."
  (when (and (mortal/frame-usable-p)
             (not (frame-parameter nil 'fullscreen)))
    (setq mortal/layout-normal-frame
          (mapcar (lambda (p) (cons p (frame-parameter nil p)))
                  mortal/frame-geometry-params))))

(defun mortal/position-on-screen-p (left top)
  "Non-nil if LEFT/TOP is a position that lies on one of the monitors."
  (and (integerp left) (integerp top)
       (seq-some (lambda (m)
                   (pcase (alist-get 'geometry m)
                     (`(,x ,y ,w ,h)
                      (and (>= left (- x 100)) (< left (+ x w -100))
                           (>= top (- y 50)) (< top (+ y h -50))))))
                 (display-monitor-attributes-list))))

(defun mortal/frame-state ()
  (when (mortal/frame-usable-p)
    (mortal/frame-note-geometry)
    (list :frame mortal/layout-normal-frame
          :fullscreen (frame-parameter nil 'fullscreen))))

(defun mortal/restore-frame (data)
  (when (mortal/frame-usable-p)
    (let* ((saved (plist-get data :frame))
           (geom (seq-filter (lambda (p) (memq (car p) mortal/frame-geometry-params))
                             saved))
           ;; Older layout files kept `fullscreen' inside :frame.
           (fullscreen (if (plist-member data :fullscreen)
                           (plist-get data :fullscreen)
                         (cdr (assq 'fullscreen saved)))))
      ;; A monitor may have gone away since the layout was saved.
      (unless (mortal/position-on-screen-p (alist-get 'left geom) (alist-get 'top geom))
        (setq geom (seq-remove (lambda (p) (memq (car p) '(left top))) geom)))
      ;; Geometry first, then maximize, so un-maximizing later returns to it.
      (when geom (modify-frame-parameters nil geom))
      (modify-frame-parameters nil `((fullscreen . ,fullscreen)))
      (setq mortal/layout-normal-frame geom))))

;;;; Windows

(defun mortal/map-state-buffers (fn state)
  "Return window STATE with every buffer name N replaced by (funcall FN N)."
  (cond
   ((and (consp state) (eq (car state) 'buffer) (stringp (cadr state)))
    (cons 'buffer (cons (funcall fn (cadr state)) (cddr state))))
   ((and (consp state) (memq (car state) '(prev-buffers next-buffers))
         (listp (cdr state)))
    (cons (car state)
          (mapcar (lambda (e)
                    (if (and (consp e) (stringp (car e)))
                        (cons (funcall fn (car e)) (cdr e))
                      e))
                  (cdr state))))
   ((consp state)
    (cons (mortal/map-state-buffers fn (car state))
          (mortal/map-state-buffers fn (cdr state))))
   (t state)))

(defun mortal/restore-windows (data)
  (let* ((names (plist-get data :names))
         (state (mortal/map-state-buffers
                 (lambda (name)
                   (let* ((file (cdr (assoc name names)))
                          (buf (and file (mortal/key-buffer file t))))
                     (if buf (buffer-name buf) name)))
                 (plist-get data :main))))
    (window-state-put state (window-main-window) 'safe)))

(defun mortal/restore-side-window (spec)
  (when-let* ((buf (mortal/key-buffer (plist-get spec :key) t)))
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
        w 'tab-line-buffers
        (delq nil (mapcar (lambda (k) (mortal/key-buffer k t)) keys)))))
   'no-mini))

;;;; Save

(defun mortal/layout-saveable-p ()
  (and mortal/layout-ready
       (not mortal/layout-restoring)
       ;; Don't save the daemon's own invisible frame, or a child frame.
       (not (and (daemonp) (not (frame-parameter nil 'client))))
       (not (frame-parameter nil 'parent-frame))))

(defun mortal/layout-save ()
  "Write the current frame/window/tab layout to `mortal/layout-file'."
  (interactive)
  (when (mortal/layout-saveable-p)
    (mortal/logged
     (walk-windows (lambda (w)
                     (set-window-parameter w 'mortal-tabs (mortal/window-tab-keys w)))
                   'no-mini)
     (let ((tmp (concat mortal/layout-file ".tmp")))
       ;; Write to a temp file and rename, so being killed mid-write can't
       ;; leave a truncated layout behind.
       (with-temp-file tmp
         (let ((print-length nil) (print-level nil))
           (prin1 (append (mortal/frame-state)
                          (list :speedbar (mortal/speedbar-open-p)
                                :sides (mortal/side-window-specs)
                                :names (mortal/file-buffer-names)
                                :main (window-state-get (window-main-window) t)))
                  (current-buffer))))
       (rename-file tmp mortal/layout-file t)))))

(defun mortal/layout-schedule-save (&rest _)
  "Save the layout once Emacs has been idle for a moment."
  (when (and mortal/layout-ready (not mortal/layout-restoring))
    (when (timerp mortal/layout-save-timer)
      (cancel-timer mortal/layout-save-timer))
    (setq mortal/layout-save-timer
          (run-with-idle-timer 2 nil #'mortal/layout-save))))

(defun mortal/layout-save-on-delete-frame (frame)
  (when (eq frame (selected-frame))
    (mortal/layout-save)))

;;;; Restore

(defun mortal/layout-restore ()
  "Restore the layout saved in `mortal/layout-file'."
  (let ((mortal/layout-restoring t)
        (cli-buffers (seq-filter #'buffer-file-name
                                 (mapcar #'window-buffer
                                         (window-list nil 'no-mini))))
        (failed nil)
        data)
    (when (file-exists-p mortal/layout-file)
      (unless (mortal/logged
               (setq data (with-temp-buffer
                            (insert-file-contents mortal/layout-file)
                            (read (current-buffer)))))
        (setq failed t))
      (when data
        (dolist (step (list (lambda () (mortal/restore-frame data))
                            (lambda () (mortal/restore-windows data))
                            (lambda () (mapc #'mortal/restore-side-window
                                             (plist-get data :sides)))
                            #'mortal/restore-tabs
                            (lambda () (when (plist-get data :speedbar)
                                         (speedbar-window-mode 1)))))
          (unless (mortal/logged (funcall step))
            (setq failed t)))
        ;; A file named on the command line wins over the saved layout.
        (mortal/logged
         (when-let* ((buf (car (last cli-buffers))))
           (unless (get-buffer-window buf t)
             (when (window-parameter (selected-window) 'window-side)
               (select-window (window-main-window)))
             (switch-to-buffer buf))))
        (force-mode-line-update t))
      (when failed
        ;; Keep what we couldn't read/apply for inspection; the next save
        ;; replaces layout.eld with the current (working) layout.
        (mortal/logged (copy-file mortal/layout-file
                                  (concat mortal/layout-file ".bad") t))
        (message "mortal layout: restore incomplete, old layout kept in %s.bad"
                 (file-name-nondirectory mortal/layout-file))))
    (setq mortal/layout-ready t)))

(defun mortal/layout-restore-in-new-daemon-frame ()
  "Under a daemon, restore the layout when the first graphical frame appears."
  (when (and (display-graphic-p)
             (not (seq-some (lambda (f)
                              (and (not (eq f (selected-frame)))
                                   (display-graphic-p f)
                                   (frame-parameter f 'client)))
                            (frame-list))))
    (mortal/layout-restore)))

(add-hook 'kill-emacs-hook #'mortal/layout-save)
(add-hook 'delete-frame-functions #'mortal/layout-save-on-delete-frame)
(add-hook 'window-configuration-change-hook #'mortal/layout-schedule-save)
(add-hook 'window-size-change-functions #'mortal/frame-note-geometry)
(add-hook 'window-size-change-functions #'mortal/layout-schedule-save)
(add-hook 'move-frame-functions #'mortal/frame-note-geometry)
(add-hook 'move-frame-functions #'mortal/layout-schedule-save)
(if (daemonp)
    (add-hook 'server-after-make-frame-hook #'mortal/layout-restore-in-new-daemon-frame)
  (add-hook 'window-setup-hook #'mortal/layout-restore))
;; Tab order changes don't trigger the hooks above, so also save periodically.
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
