;;; -*- lexical-binding: t; -*-

;;; mortal-mode.el --- Mortal minor mode

;;; ---------------------------------------------------------------------------
;;; Dependencies
;;; ---------------------------------------------------------------------------

(require 'mortal-keymap)
(require 'which-key)

(declare-function mortal-response-keymap-mode "mortal-keymap")
(declare-function speedbar-mode "speedbar")
(declare-function speedbar-window-mode "speedbar")
(declare-function speedbar-find-file-in-frame "speedbar")

(defvar speedbar--buffer-name)
(defvar speedbar-buffer)
(defvar speedbar-prefer-window)
(defvar speedbar-window-default-width)
(defvar speedbar-window-max-width)


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


(defun mortal/speedbar-find-file (file)
  "Open FILE in the main window instead of the Speedbar window."
  (let ((win (window-main-window)))
    (if (window-live-p win)
        (select-window win)
      (other-window 1)))
  (find-file file))


;;; ---------------------------------------------------------------------------
;;; Mortal mode
;;; ---------------------------------------------------------------------------

(define-minor-mode mortal-mode
  "A global minor mode for mortal-related keybindings."
  :global t
  :lighter " Mortal"
  :group 'mortal

  (let ((state (if mortal-mode 1 -1)))

    ;; -------------------------------------------------------------------------
    ;; Editing behavior
    ;; -------------------------------------------------------------------------

    (electric-indent-mode state)
    (electric-quote-mode state)
    (electric-pair-mode state)
    (electric-layout-mode state)


    ;; -------------------------------------------------------------------------
    ;; Mortal response keymap
    ;; -------------------------------------------------------------------------

    (mortal-response-keymap-mode state)


    ;; -------------------------------------------------------------------------
    ;; Mortal keymap with emulation priority
    ;; -------------------------------------------------------------------------

    (if mortal-mode
        (add-to-list 'emulation-mode-map-alists
                     `((mortal-mode . ,mortal-map)))
      (setq emulation-mode-map-alists
            (assq-delete-all 'mortal-mode
                             emulation-mode-map-alists)))


    ;; -------------------------------------------------------------------------
    ;; Tab-line
    ;; -------------------------------------------------------------------------

    (global-tab-line-mode state)


    ;; -------------------------------------------------------------------------
    ;; Which-key
    ;; -------------------------------------------------------------------------

    (which-key-mode state)

    (when mortal-mode
      (dolist (entry (accessible-keymaps global-map))
        (let ((keymap (cdr entry)))
          (unless (lookup-key keymap (kbd "<next>"))
            (define-key keymap
                        (kbd "<next>")
                        #'which-key-show-next-page-cycle))
          (unless (lookup-key keymap (kbd "<prior>"))
            (define-key keymap
                        (kbd "<prior>")
                        #'which-key-show-previous-page-cycle)))))


    ;; -------------------------------------------------------------------------
    ;; Speedbar
    ;; -------------------------------------------------------------------------

    (if mortal-mode
        (progn
          (require 'speedbar)

          (setq speedbar-prefer-window t
                speedbar-window-default-width 25
                speedbar-window-max-width 25)

          ;; Emacs 31.1 mouse fix.
          (unless (advice-member-p #'mortal/speedbar-fix
                                   #'speedbar-window-mode)
            (advice-add #'speedbar-window-mode
                        :before
                        #'mortal/speedbar-fix))

          ;; Open Speedbar files in the main window.
          (unless (advice-member-p #'mortal/speedbar-find-file
                                   #'speedbar-find-file-in-frame)
            (advice-add #'speedbar-find-file-in-frame
                        :override
                        #'mortal/speedbar-find-file)))

      ;; Remove Speedbar advice when Mortal is disabled.
      (when (featurep 'speedbar)
        (advice-remove #'speedbar-window-mode
                       #'mortal/speedbar-fix)
        (advice-remove #'speedbar-find-file-in-frame
                       #'mortal/speedbar-find-file)))))


;;; ---------------------------------------------------------------------------
;;; End
;;; ---------------------------------------------------------------------------

(provide 'mortal-mode)
;;; mortal-mode.el ends here
