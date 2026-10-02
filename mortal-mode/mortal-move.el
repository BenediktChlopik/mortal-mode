;;; mortal-move.el --- Smart movement functions -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar mortal-move-style)


;; add more flavours of movement functions, so the user can select what he preffers

(defun mortal/move-smart (dir)
  "Move point one \"smart\" step in DIR (1 = forward, -1 = backward).

Rules (stated for DIR = 1; mirror for DIR = -1):

1. Every character belongs to one of three groups: word characters
   ([A-Za-z0-9]), special characters (everything else except
   tabs/spaces/newline), and whitespace (tabs/spaces).

2. If point is not already at a \"stop\" (see rule 4), a step always
   crosses two of those three groups: it skips over the run of the
   group at point, and then continues and skips over the following
   run, that belongs to a different group. This applies whether the
   group at point is a word/special run or a whitespace run.

3. While crossing that second group, do NOT cross a newline: if the
   second group is whitespace and it is the line's trailing
   whitespace (i.e. crossing it would put point at the end of the
   line), stop right before it instead -- crossing only the first
   group.

4. If point is already at such a stop (only tabs/spaces, or nothing,
   between point and the end of the line), then this call crosses
   the newline and lands at the tab indent of the next line."
  (cl-labels
      ((skip (chars)
         (if (> dir 0) (skip-chars-forward chars)
           (skip-chars-backward chars)))
       (at-edge-p ()
         (if (> dir 0)
             (looking-at "[ \t]*$")
           (looking-back "^[ \t]*" (line-beginning-position))))
       (class-at ()
         (let ((ch (if (> dir 0) (char-after) (char-before))))
           (cond
            ((null ch) nil)
            ((eq ch ?\n) nil)
            ((string-match-p "[A-Za-z0-9]" (string ch)) 'word)
            ((string-match-p "[ \t]" (string ch)) 'ws)
            (t 'special))))
       (class-chars (class)
         (pcase class
           ('word "A-Za-z0-9")
           ('ws " \t")
           ('special "^A-Za-z0-9 \t\n"))))
    (cond
     ((at-edge-p)
      (if (> dir 0)
          (progn
            (forward-line 1)
            (skip-chars-forward " \t"))
        (when (zerop (forward-line -1))
          (end-of-line)
          (skip-chars-backward " \t"))))
     (t
      (let ((c1 (class-at)))
        (skip (class-chars c1))
        (let ((c2 (class-at)))
          (when (and c2 (not (at-edge-p)))
            (skip (class-chars c2)))))))))

(defun mortal/move-gecko (dir)
  "Move point one \"gecko\" step in DIR (1 = forward, -1 = backward).
1. Every character is one of three groups: **word** (`A-Z`, `a-z`, `0-9`),
   **special** (everything else except space/tab/newline), or **whitespace**
   (space/tab/newline).

2. Consecutive characters of the same group form a run.

3. Movement skips the current run, then the adjacent run in the direction
   of movement.

4. When moving forward, a **word → special** transition creates a stop
   after the special run.

5. When moving backward, a **special → word** transition creates a stop
   before the special run.

6. A special run immediately adjacent to whitespace does not create an
   additional punctuation stop.

7. Consecutive special characters are treated as one run; `...`, `::`,
   `---`, etc. are not split internally.

8. When the second run contains a newline, movement stops at the newline
   rather than crossing it.

9. From that newline stop, the next movement crosses the newline and lands
   at the indentation of the adjacent line.
"
  (let ((step (if (> dir 0) 1 -1))
        (phase 'start)   ; start -> run1 -> run2 -> (trail) ; or start -> indent
        (cur nil)        ; group of the first (current) run
        (adj nil)        ; group of the second (adjacent) run
        (done nil))
    (while (not done)
      (let* ((pos (if (> step 0) (point) (1- (point))))
             (c (and (>= pos (point-min))
                     (< pos (point-max))
                     (char-after pos)))
             ;; Group of the next character in the direction of movement.
             (g (cond ((null c) nil)
                      ((eq c ?\n) 'nl)
                      ((or (eq c ?\s) (eq c ?\t)) 'ws)
                      ((or (and (>= c ?a) (<= c ?z))
                           (and (>= c ?A) (<= c ?Z))
                           (and (>= c ?0) (<= c ?9)))
                       'word)
                      (t 'sym))))
        (cond
         ;; Buffer edge reached.
         ((null g)
          (setq done t))

         ;; Rule 9: sitting at a newline stop -> cross it, then land at the
         ;; indentation (skip blanks) of the adjacent line.
         ((eq phase 'start)
          (if (eq g 'nl)
              (progn (forward-char step)
                     (setq phase 'indent))
            (setq cur g
                  phase 'run1)))

         ;; Rule 3, part 1: skip the current run.
         ;; Rule 8: a newline ends movement (stop at it, don't cross).
         ((eq phase 'run1)
          (cond ((eq g cur) (forward-char step))
                ((eq g 'nl) (setq done t))
                (t (setq adj g
                         phase 'run2))))

         ;; Rule 3, part 2: skip the adjacent run.
         ;; Rule 6: a special run followed by blanks gets no stop of its own
         ;; (rules 4/5), so keep going over the blanks.
         ((eq phase 'run2)
          (cond ((eq g adj) (forward-char step))
                ((and (eq adj 'sym) (eq g 'ws)) (setq phase 'trail))
                (t (setq done t))))

         ;; Skip blanks (after a special run, or as indentation).
         ;; Stops at a newline or at the first non-blank character.
         (t
          (if (eq g 'ws)
              (forward-char step)
            (setq done t))))))))

(defun mortal/move (dir)
  "Move point according to `mortal-move-style'."
  (pcase mortal-move-style
    ('vanilla
     (if (> dir 0)
         (progn
           (forward-word 1))
       (backward-word 1)))

    ('smart
     (mortal/move-smart dir))
    
    ('gecko
     (mortal/move-gecko dir))

    (_
     (user-error "Invalid `mortal-move-style': %S"
                 mortal-move-style))))


(provide 'mortal-move)
;;; mortal-move.el ends here
