;;; -*- lexical-binding: t; -*-

;;; mortal-jumper.el --- Mouse click history for Mortal

;;; Commentary:
;;
;; Tracks the positions of mouse clicks (via `mouse-set-point') in each
;; buffer, and lets you cycle back and forth through that history with
;; `mortal-jumper-next' and `mortal-jumper-previous'.
;;
;; Each history entry records not just where point was, but the full
;; view at that moment: the window's vertical scroll (`window-start')
;; and horizontal scroll (`window-hscroll'). Jumping to an entry
;; restores all of it, so the buffer looks exactly as it did when the
;; entry was recorded, not just point landing in the right spot.
;;
;; History is stored per-buffer in `mortal-jumper-history', newest
;; entry first. `mortal-jumper-index' tracks where in that list you
;; currently are while cycling; it is reset to -1 (i.e. "not cycling")
;; whenever a new click is recorded.
;;
;; Note on direction: because new entries are pushed to the front of
;; the list, calling `mortal-jumper-next' repeatedly walks from the
;; most recent entry towards older ones, and `mortal-jumper-previous'
;; walks back towards more recent ones. If you expect "next" to mean
;; "more recent", swap the two function bodies.
;;
;; The buffer's very first view — wherever point was before you ever
;; clicked in it, e.g. right after opening the file — is captured
;; automatically the first time you click anywhere in the buffer (see
;; `mortal-jumper--record-initial-view'), so it becomes the oldest
;; entry in the history. On top of that, the first time you call
;; either command since the last click, your current view at that
;; moment is also recorded (see `mortal-jumper--maybe-record-start'),
;; so cycling back with `mortal-jumper-previous' will always be able
;; to return you to exactly where you started navigating from. A view
;; is never recorded twice in a row for the same point position.
;; Neither command wraps around at the ends of the history.

;;; Code:

(require 'cl-lib)

;;; ---------------------------------------------------------------------------
;;; Variables
;;; ---------------------------------------------------------------------------

(cl-defstruct mortal-jumper-entry
  "A single recorded view in `mortal-jumper-history'.
POINT is a marker at the recorded buffer position. WINDOW-START is a
marker at the recorded top-of-window position (vertical scroll).
HSCROLL is the recorded horizontal scroll amount, as returned by
`window-hscroll'."
  point
  window-start
  hscroll)

(defvar-local mortal-jumper-history nil
  "List of recorded views (`mortal-jumper-entry' structs) in the current buffer.
Newest entry is stored first (position 0).")

(defvar-local mortal-jumper-index -1
  "Current cursor position within `mortal-jumper-history'.
A value of -1 means no cycling is currently in progress; the next
call to `mortal-jumper-next' or `mortal-jumper-previous' will start
from the most recently recorded entry.")


;;; ---------------------------------------------------------------------------
;;; Shared helpers
;;; ---------------------------------------------------------------------------

(defun mortal-jumper--entry-point-at-p (entry pos)
  "Return non-nil if ENTRY's recorded point is at POS in the current buffer.
ENTRY may be nil (e.g. when the history is empty), in which case this
is nil. Used to avoid recording the same buffer position twice in a
row."
  (and entry
       (let ((marker (mortal-jumper-entry-point entry)))
         (and marker
              (eq (marker-buffer marker) (current-buffer))
              (= (marker-position marker) pos)))))

(defun mortal-jumper--capture-view (window pos)
  "Capture the current view of WINDOW as a `mortal-jumper-entry'.
POS is the buffer position to record as the entry's point (this is
taken as a parameter, rather than read from `point', so callers can
pass an exact click position). WINDOW's current `window-start' and
`window-hscroll' are recorded alongside it, so the entry can later be
used to restore the full view, not just point."
  (make-mortal-jumper-entry
   :point (copy-marker pos)
   :window-start (copy-marker (window-start window))
   :hscroll (window-hscroll window)))

(defun mortal-jumper--restore-entry (entry)
  "Restore the view saved in ENTRY in the selected window.
Moves point to ENTRY's recorded position, then restores the window's
vertical scroll (`window-start') and horizontal scroll
(`window-hscroll') to match how the view looked when ENTRY was
recorded."
  (goto-char (mortal-jumper-entry-point entry))
  (set-window-start (selected-window)
                    (mortal-jumper-entry-window-start entry)
                    nil)
  (set-window-hscroll (selected-window)
                      (mortal-jumper-entry-hscroll entry)))


;;; ---------------------------------------------------------------------------
;;; Recording
;;; ---------------------------------------------------------------------------

(defun mortal-jumper--record-initial-view (event &rest _)
  "Record the buffer's pre-click view, before EVENT's click moves point.

Intended to be advised as `:before' onto `mouse-set-point' (see the
`advice-add' call below), so this runs immediately before EVENT's
click is processed — while point, `window-start', and `window-hscroll'
in the target window still reflect wherever things were beforehand.

If EVENT resolves to a live window, and that window's buffer has no
`mortal-jumper-history' entries yet, the buffer's current
(pre-click) view is captured as the oldest entry in its history, via
`mortal-jumper--capture-view'. This preserves the very first position
you were at in the buffer — even one you never explicitly clicked or
navigated to yourself, such as wherever the buffer was left when it
was opened.

Does nothing if EVENT does not resolve to a window, or if the
buffer's history already has entries (meaning its initial view was
already captured by an earlier click or navigation)."
  (when-let* ((posn (event-end event))
              (window (posn-window posn)))
    (when (windowp window)
      (with-current-buffer (window-buffer window)
        (when (null mortal-jumper-history)
          (push (mortal-jumper--capture-view window (window-point window))
                mortal-jumper-history))))))


(defun mortal-jumper-record-click (event &rest _)
  "Record the view at mouse click EVENT.

Intended to be advised onto `mouse-set-point' (see `advice-add' call
below), so EVENT is whatever mouse event triggered that command.

If EVENT resolves to a live window and a valid buffer position, an
entry capturing that position along with the window's current scroll
(see `mortal-jumper--capture-view') is pushed onto
`mortal-jumper-history' in the buffer the click occurred in, and
`mortal-jumper-index' is reset to -1 so a subsequent jump starts from
this newest entry. If that position is already the frontmost entry in
`mortal-jumper-history' (e.g. a double click, or a click that lands
where the last click was), no duplicate entry is pushed.

Does nothing if EVENT does not resolve to a window/position (e.g.
clicks on the mode line or other non-buffer areas)."
  (when-let* ((posn (event-end event))
              (window (posn-window posn))
              (point (posn-point posn)))
    (when (and (windowp window)
               (integer-or-marker-p point))
      (with-current-buffer (window-buffer window)
        (unless (mortal-jumper--entry-point-at-p (car mortal-jumper-history) point)
          (push (mortal-jumper--capture-view window point)
                mortal-jumper-history))
        (setq mortal-jumper-index -1)))))


(advice-add #'mouse-set-point
            :before
            #'mortal-jumper--record-initial-view)

(advice-add #'mouse-set-point
            :after
            #'mortal-jumper-record-click)


;;; ---------------------------------------------------------------------------
;;; Jumping
;;; ---------------------------------------------------------------------------

(defun mortal-jumper--maybe-record-start ()
  "Record the current view as a history entry if a new navigation is starting.

If `mortal-jumper-index' is -1 (meaning no navigation is currently in
progress), set `mortal-jumper-index' to 0 to point at the frontmost
entry of `mortal-jumper-history'. If point is not already at that
frontmost entry's recorded position — for example because you moved
with the keyboard rather than the mouse since the last recorded click
— capture the current view (see `mortal-jumper--capture-view') and
push it onto the front of the history first, so it becomes that
frontmost entry.

This ensures the view you were at before you started cycling through
`mortal-jumper-next'/`mortal-jumper-previous' is itself preserved in
the history (without ever being recorded twice), so you can always
navigate back to exactly where you started."
  (when (= mortal-jumper-index -1)
    (unless (mortal-jumper--entry-point-at-p (car mortal-jumper-history) (point))
      (push (mortal-jumper--capture-view (selected-window) (point))
            mortal-jumper-history))
    (setq mortal-jumper-index 0)))


(defun mortal-jumper-next ()
  "Jump to the next recorded view.

If this is the first navigation call since the last click — or the
very first call ever in this buffer, before any click has been made —
the current view is first recorded as an entry in
`mortal-jumper-history' (see `mortal-jumper--maybe-record-start'), so
it can be returned to later.

Advances `mortal-jumper-index' by one position in
`mortal-jumper-history' and restores the view there — both point and
the window's scroll position (see `mortal-jumper--restore-entry').
Since history is ordered newest-first, repeated calls walk from the
most recent entry towards progressively older ones.

Signals a `user-error' if already at the oldest recorded entry (does
not wrap around)."
  (interactive)
  (mortal-jumper--maybe-record-start)
  (if (>= (1+ mortal-jumper-index) (length mortal-jumper-history))
      (user-error "No older recorded clicks")
    (setq mortal-jumper-index (1+ mortal-jumper-index))
    (mortal-jumper--restore-entry
     (nth mortal-jumper-index mortal-jumper-history))))


(defun mortal-jumper-previous ()
  "Jump to the previous recorded view.

If this is the first navigation call since the last click — or the
very first call ever in this buffer, before any click has been made —
the current view is first recorded as an entry in
`mortal-jumper-history' (see `mortal-jumper--maybe-record-start'), so
it can be returned to later. In that case, calling this command
immediately signals a `user-error' since there is nothing more recent
than your starting view.

Moves `mortal-jumper-index' back by one position in
`mortal-jumper-history' and restores the view there — both point and
the window's scroll position (see `mortal-jumper--restore-entry').
Since history is ordered newest-first, repeated calls walk from the
current position towards progressively more recent entries.

Signals a `user-error' if already at the most recent recorded entry
(does not wrap around)."
  (interactive)
  (mortal-jumper--maybe-record-start)
  (if (<= (1- mortal-jumper-index) -1)
      (user-error "No more recent recorded clicks")
    (setq mortal-jumper-index (1- mortal-jumper-index))
    (mortal-jumper--restore-entry
     (nth mortal-jumper-index mortal-jumper-history))))


;;; ---------------------------------------------------------------------------
;;; End
;;; ---------------------------------------------------------------------------

(provide 'mortal-jumper)
;;; mortal-jumper.el ends here
