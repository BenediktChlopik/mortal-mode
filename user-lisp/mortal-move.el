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

    (_
     (user-error "Invalid `mortal-move-style': %S"
                 mortal-move-style))))


(provide 'mortal-move)
;;; mortal-move.el ends here
