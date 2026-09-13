;;; mortal-jumper.el --- Minimal mouse jump history -*- lexical-binding: t; -*-

;;; Code:

(defvar mortal-jumper-history '(nil . nil)
  "Backward and forward mouse jump history.")

(defun mortal-jumper/mouse-set-point (event)
  "Save point and move it with mouse-1."
  (interactive "e")
  (unless (string-prefix-p "*" (buffer-name))
    (push (cons (current-buffer) (point))
          (car mortal-jumper-history))
    (setcdr mortal-jumper-history nil))
  (mouse-set-point event))

(defun mortal-jumper/jump-back ()
  "Jump backward through mouse positions."
  (interactive)
  (unless (car mortal-jumper-history)
    (user-error "No previous jump"))
  (push (cons (current-buffer) (point))
        (cdr mortal-jumper-history))
  (let ((pos (pop (car mortal-jumper-history))))
    (deactivate-mark)
    (switch-to-buffer (car pos))
    (goto-char (cdr pos))))

(defun mortal-jumper/jump-forward ()
  "Jump forward through mouse positions."
  (interactive)
  (unless (cdr mortal-jumper-history)
    (user-error "No next jump"))
  (push (cons (current-buffer) (point))
        (car mortal-jumper-history))
  (let ((pos (pop (cdr mortal-jumper-history))))
    (deactivate-mark)
    (switch-to-buffer (car pos))
    (goto-char (cdr pos))))

(provide 'mortal-jumper)

;;; mortal-jumper.el ends here
