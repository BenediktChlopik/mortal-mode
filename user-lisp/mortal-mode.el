;;; -*- lexical-binding: t; -*-

;;; mortal-mode.el --- Mortal minor mode

;;; ---------------------------------------------------------------------------
;;; Dependencies
;;; ---------------------------------------------------------------------------

(require 'mortal-keymap)

(declare-function mortal-response-keymap-mode "mortal-keymap")


;;; ---------------------------------------------------------------------------
;;; Mortal mode
;;; ---------------------------------------------------------------------------

(define-minor-mode mortal-mode
  "A global minor mode for mortal-related keybindings."
  :global t
  :lighter " Mortal"
  :group 'mortal
  (let ((state (if mortal-mode 1 -1)))
    ;; Editing behavior
    (electric-indent-mode state)
    (electric-quote-mode state)
    (electric-pair-mode state)
    (electric-layout-mode state)

    ;; Mortal response keymap
    (mortal-response-keymap-mode state)

    ;; Mortal keymap with emulation priority
    (if mortal-mode
        (add-to-list 'emulation-mode-map-alists
                     `((mortal-mode . ,mortal-map)))
      (setq emulation-mode-map-alists
            (assq-delete-all 'mortal-mode
                             emulation-mode-map-alists)))

    ;; Which-key
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
                        #'which-key-show-previous-page-cycle)))))))

;;; ---------------------------------------------------------------------------
;;; End
;;; ---------------------------------------------------------------------------

(provide 'mortal-mode)
;;; mortal-mode.el ends here
