;;; mortal-jumper.el --- Minimal mouse jump history -*- lexical-binding: t; -*-

;;; Code:

(defvar-local mortal-jumper-history (cons nil nil)
  "Backward and forward mouse jump history, local to each buffer.
Each element of the car and cdr lists is a marker, so history
entries remain valid across insertions and deletions elsewhere
in the buffer.")
(put 'mortal-jumper-history 'permanent-local t)

(defun mortal-jumper--make-marker (buf pt)
  "Return a new marker in BUF at PT."
  (let ((marker (make-marker)))
    (set-marker marker pt buf)
    marker))

(defun mortal-jumper--free-marker (marker)
  "Detach MARKER from its buffer so it stops tracking edits."
  (when (markerp marker)
    (set-marker marker nil)))

(defun mortal-jumper--live-marker-position (marker)
  "Return MARKER's position if it still points into a live buffer, else nil."
  (and (markerp marker)
       (marker-buffer marker)
       (buffer-live-p (marker-buffer marker))
       (marker-position marker)))

(defun mortal-jumper--maybe-record (event)
  "Record a jump-history entry for a plain single mouse-1 click.
Recording happens in the buffer the click landed in, not
whatever buffer happened to be current before the click was
processed.  Consecutive clicks at the same position are not
recorded twice."
  (when (and (eq (event-basic-type event) 'mouse-1)
             (= (event-click-count event) 1))
    (let* ((posn (event-start event))
           (win (posn-window posn))
           (buf (and (windowp win) (window-buffer win)))
           (pt (posn-point posn)))
      (when (and buf pt (not (string-prefix-p "*" (buffer-name buf))))
        (with-current-buffer buf
          (unless (eq pt (mortal-jumper--live-marker-position (car (car mortal-jumper-history))))
            (push (mortal-jumper--make-marker buf pt) (car mortal-jumper-history))
            (mapc #'mortal-jumper--free-marker (cdr mortal-jumper-history))
            (setcdr mortal-jumper-history nil)))))))

(defun mortal-jumper--mouse-set-point-advice (orig-fn event &optional promote-to-region)
  (mortal-jumper--maybe-record event)
  (funcall orig-fn event promote-to-region))

(advice-add 'mouse-set-point :around #'mortal-jumper--mouse-set-point-advice)

(defun mortal-jumper--rotate (from-slot to-slot)
  "Move all markers from FROM-SLOT of history into TO-SLOT, reversed.
FROM-SLOT and TO-SLOT are `car' or `cdr'.  Used to wrap navigation
around the far end of history when the near end is exhausted."
  (funcall (if (eq to-slot 'car) #'setcar #'setcdr)
           mortal-jumper-history
           (nreverse (funcall from-slot mortal-jumper-history)))
  (funcall (if (eq from-slot 'car) #'setcar #'setcdr)
           mortal-jumper-history nil))

(defun mortal-jumper/jump-back ()
  "Jump backward through mouse positions in the current buffer.
Wraps around to the most recent forward position when the
backward history is exhausted."
  (interactive)
  (when (null (car mortal-jumper-history))
    (if (cdr mortal-jumper-history)
        (mortal-jumper--rotate 'cdr 'car)
      (user-error "No jump history")))
  (unless (eq (point) (mortal-jumper--live-marker-position (car (cdr mortal-jumper-history))))
    (push (mortal-jumper--make-marker (current-buffer) (point)) (cdr mortal-jumper-history)))
  (deactivate-mark)
  (let* ((marker (pop (car mortal-jumper-history)))
         (pos (mortal-jumper--live-marker-position marker)))
    (mortal-jumper--free-marker marker)
    (if pos
        (goto-char pos)
      (mortal-jumper/jump-back))))

(defun mortal-jumper/jump-forward ()
  "Jump forward through mouse positions in the current buffer.
Wraps around to the oldest backward position when the forward
history is exhausted."
  (interactive)
  (when (null (cdr mortal-jumper-history))
    (if (car mortal-jumper-history)
        (mortal-jumper--rotate 'car 'cdr)
      (user-error "No jump history")))
  (unless (eq (point) (mortal-jumper--live-marker-position (car (car mortal-jumper-history))))
    (push (mortal-jumper--make-marker (current-buffer) (point)) (car mortal-jumper-history)))
  (deactivate-mark)
  (let* ((marker (pop (cdr mortal-jumper-history)))
         (pos (mortal-jumper--live-marker-position marker)))
    (mortal-jumper--free-marker marker)
    (if pos
        (goto-char pos)
      (mortal-jumper/jump-forward))))

(provide 'mortal-jumper)

;;; mortal-jumper.el ends here
