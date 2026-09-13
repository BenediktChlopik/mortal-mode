;;; mortal-jumper.el --- Minimal pop-to-mark jump history -*- lexical-binding: t; -*-

;;; Code:

(defvar-local mortal-jumper--history nil
  "Locations saved before `mortal-jumper/pop-to-mark'.")

(defun mortal-jumper/pop-to-mark ()
  "Pop to the next mark and remember the current location."
  (interactive)
  (unless (equal (point) (car mortal-jumper--history))
    (push (point) mortal-jumper--history))
  (deactivate-mark)
  (call-interactively #'pop-to-mark-command))

(defun mortal-jumper/jump-back ()
  "Return to the previous location in the current buffer."
  (interactive)
  (if-let* ((point (pop mortal-jumper--history)))
      (goto-char point)
    (user-error "No previous jump")))

(provide 'mortal-jumper)

;;; mortal-jumper.el ends here
