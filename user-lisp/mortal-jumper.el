;;; mortal-jumper.el --- Minimal mouse jump history -*- lexical-binding: t; -*-

;;; Code:

(defvar mortal-jumper-history '(nil . nil)
  "Backward and forward mouse jump history.")

(defun mortal-jumper/mouse-set-point (event)
  "Save point and move it with mouse-1.
Only record jump history for plain single clicks; double/triple
clicks are word/line selections, not navigation jumps.  Passing
promote-to-region' to mouse-set-point' lets it correctly extend
the selection on multi-clicks instead of deactivating the mark
that `mouse-drag-region' just set."
  (interactive "e")
  (when (and (eq (car event) 'mouse-1)
             (not (string-prefix-p "*" (buffer-name))))
    (push (cons (current-buffer) (point))
          (car mortal-jumper-history))
    (setcdr mortal-jumper-history nil))
  (mouse-set-point event 'promote-to-region))

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
