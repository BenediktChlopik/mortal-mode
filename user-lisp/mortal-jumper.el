;;; mortal-jumper.el --- Minimal pop-to-mark jump history -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal way to undo pop-to-mark-command.
;;
;; Use `mortal-jumper/pop-to-mark' instead of `pop-to-mark-command',
;; then `mortal-jumper/jump-back' returns to the previous location.

;;; Code:

(defvar-local mortal-jumper--history nil
  "Locations saved before `mortal-jumper/pop-to-mark'.")

(defun mortal-jumper/pop-to-mark ()
  "Pop to mark, remembering the current location."
  (interactive)
  (push (point) mortal-jumper--history)
  (call-interactively #'pop-to-mark-command))

(defun mortal-jumper/jump-back ()
  "Return to the previous location in the current buffer."
  (interactive)
  (if-let* ((point (pop mortal-jumper--history)))
      (goto-char point)
    (user-error "No previous jump")))

(provide 'mortal-jumper)

;;; mortal-jumper.el ends here
