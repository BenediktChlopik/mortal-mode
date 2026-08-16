;; -*- lexical-binding: t; -*-

(defun mortal/save-and-close-file ()
  "Save the current buffer, and close the window."
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (delete-window))

(defun mortal/new-file-buffer ()
  "Open a new tab containing a fresh, unsaved buffer.
The buffer is not associated with any file on disk; you will
be prompted for a filename the first time you save it."
  (interactive)
  (tab-bar-new-tab)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (setq buffer-offer-save t)
    buffer))


(defun mortal/copy-line-or-region ()
  "Copy the active region to the kill ring, or the whole current
line if no region is active. Point is moved to the start of the
line afterward."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (line-beginning-position) (line-end-position)))
  (beginning-of-line))


(defun mortal/forward-word ()
  "Skip over whitespace and move past the following word.
If a newline is crossed while skipping whitespace, cross it, then
skip over any leading whitespace on the next line, and stop there
without moving into the following word."
  (interactive)
  (skip-chars-forward " \t\r")
  (if (eq (char-after) ?\n)
      (progn
        (forward-char 1)
        (skip-chars-forward " \t\r"))
    (forward-same-syntax))) 

(defun mortal/backward-word ()
  "Skip over whitespace and move past the previous word (backward).
If a newline is crossed while skipping whitespace, cross it, then
skip over any trailing whitespace at the end of the previous line,
and stop there without moving into the preceding word."
  (interactive)
  (skip-chars-backward " \t\r")
  (if (eq (char-before) ?\n)
      (progn
        (backward-char 1)
        (skip-chars-backward " \t\r"))
    (forward-same-syntax -1)))


(defun mortal/backward-delete-word ()
  "Delete whitespace before point, then delete the word before that.
If point is preceded by whitespace, delete all contiguous whitespace first.
Then delete the previous word (without whitespace) as well."
  (interactive)
  (let ((start (point)))
    (mortal/backward-word)
    (delete-region (point) start)))

(defun mortal/forward-delete-word ()
  "Delete whitespace before point, then delete the word after that.
If point is preceded by whitespace, delete all contiguous whitespace first.
Then delete the next word (without whitespace) as well."
  (interactive)
  (let ((start (point)))
    (mortal/forward-word)
    (delete-region (point) start)))


(defun mortal/move-lines-vertically (direction)
  "Move the lines spanned by the active region one line vertically.
DIRECTION is 1 to move down, -1 to move up.  Re-marks the moved
lines afterwards so the region stays selected for repeated calls."
  (let* ((swap-point-mark (> (point) (mark)))
         (beg (save-excursion
                (goto-char (region-beginning))
                (line-beginning-position)))
         (end (save-excursion
                (goto-char (region-end))
                (if (bolp) (point) (line-beginning-position 2))))
         (text (delete-and-extract-region beg end)))
    (goto-char beg)
    (forward-line direction)
    (let ((new-beg (point)))
      (insert text)
      (let ((new-end (point)))
        (if swap-point-mark
            (progn (set-mark new-beg) (goto-char new-end))
          (progn (set-mark new-end) (goto-char new-beg)))
        (setq deactivate-mark nil)))))


(defun mortal/delete-line ()
  "Delete the current line, including its trailing newline."
  (interactive)
  (delete-region (line-beginning-position) (1+ (line-end-position))))

(defun mortal/kill-line-or-region ()
  "Kill the active region, or the whole current line if no region is active."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))


(defun mortal/insert-line-below ()
  "Insert a new line below the current line and move point there."
  (interactive)
  (end-of-line)
  (newline)
  (indent-according-to-mode))

(defun mortal/insert-line-above ()
  "Insert a new line above the current line and move point there."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-according-to-mode))


(defun mortal/isearch-forward-or-yank-region ()
  "Start `isearch-forward'. If a region is active, prefill the
search string with its contents; otherwise start isearch normally."
  (interactive)
  (if (use-region-p)
      (let ((string (buffer-substring-no-properties
                     (region-beginning) (region-end))))
        (deactivate-mark)
        (isearch-forward nil 1)
        (isearch-yank-string string))
    (isearch-forward)))

(defun mortal/query-replace-or-yank-region ()
  "Run `query-replace'. If a region is active, use its contents as
the search string and only prompt for the replacement; otherwise
start `query-replace' normally."
  (interactive)
  (if (use-region-p)
      (let* ((from (buffer-substring-no-properties
                    (region-beginning) (region-end)))
             (to (query-replace-read-to from "Query replace" nil)))
        (deactivate-mark)
        (query-replace from to))
    (call-interactively #'query-replace)))


(defvar mortal/selection-commands
  '(mortal/select-word-right
    mortal/select-word-left
    mortal/select-char-right
    mortal/select-char-left
    mark-whole-buffer
    mortal/select-line-up
    mortal/select-line-down
    mortal/select-paragraph-up
    mortal/select-paragraph-down
    mortal/move-line-up
    mortal/move-line-down)
  "Commands that are allowed to extend the region without clearing it.")

(defun mortal/move-line-up ()
  "Move the current line up.
If a region is active, move all marked lines up instead."
  (interactive)
  (if (use-region-p)
      (mortal/move-lines-vertically -1)
    (progn
      (transpose-lines 1)
      (forward-line -2))))

(defun mortal/move-line-down ()
  "Move the current line down.
If a region is active, move all marked lines down instead."
  (interactive)
  (if (use-region-p)
      (mortal/move-lines-vertically 1)
    (progn
      (forward-line 1)
      (transpose-lines 1)
      (forward-line -1))))

(defun mortal/select-word-right ()
  "Start the region if none is active, then extend it forward using `mortal/forward-word'."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (mortal/forward-word))

(defun mortal/select-word-left ()
  "Start the region if none is active, then extend it backward using `mortal/backward-word'."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (mortal/backward-word))

(defun mortal/select-char-right ()
  "Start the region if none is active, then extend it forward by one char."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (forward-char 1))

(defun mortal/select-char-left ()
  "Start the region if none is active, then extend it backward by one char."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (backward-char 1))

(defun mortal/select-line-down ()
  "Start the region if none is active, then extend it down by one line."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (let ((col (current-column)))
    (forward-line 1)
    (move-to-column col)))

(defun mortal/select-line-up ()
  "Start the region if none is active, then extend it up by one line."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (let ((col (current-column)))
    (forward-line -1)
    (move-to-column col)))

(defun mortal/select-paragraph-down ()
  "Start the region if none is active, then extend it forward by one paragraph."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (forward-paragraph 1))

(defun mortal/select-paragraph-up ()
  "Start the region if none is active, then extend it backward by one paragraph."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (backward-paragraph 1))

(defun mortal/deactivate-mark-unless-selecting ()
  "Deactivate the region unless the just-run command was one that
extends selection, or a mouse-drag/mouse-selection command."
  (unless (or (memq this-command mortal/selection-commands)
              (mouse-event-p last-command-event)
              (memq this-command '(mouse-save-then-kill
                                   mouse-set-region
                                   mouse-drag-region
                                   mouse-set-point)))
    (deactivate-mark)))

(add-hook 'post-command-hook #'mortal/deactivate-mark-unless-selecting)



(defvar mortal-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") (kbd "C-g"))

    ;; emacs prefixes
    (define-key map (kbd "<f1>") ctl-x-map)
    (define-key map (kbd "<f3>") ctl-x-r-map)
    (define-key map (kbd "<f5>") 'help-command)

    ;; window management
    (define-key map (kbd "<f2> 1") 'delete-other-windows)
    (define-key map (kbd "<f2> 2") 'split-window-right)
    (define-key map (kbd "<f2> 3") 'split-window-below)
    
    (define-key map (kbd "<f2> <left>") 'windmove-left)
    (define-key map (kbd "<f2> <right>") 'windmove-right)
    (define-key map (kbd "<f2> <up>") 'windmove-up)
    (define-key map (kbd "<f2> <down>") 'windmove-down)

    ;; tab management 
    (dotimes (i 9)
      (let ((n (1+ i)))
	(define-key map (kbd (format "C-%d" n))
		    (lambda () (interactive) (tab-bar-select-tab n)))))

    ;; select
    (define-key map (kbd "C-a") 'mark-whole-buffer)
    
    (define-key map (kbd "C-S-<left>") #'mortal/select-word-left)
    (define-key map (kbd "C-S-<right>") #'mortal/select-word-right)

    (define-key map (kbd "C-S-<up>") #'mortal/select-paragraph-up)
    (define-key map (kbd "C-S-<down>") #'mortal/select-paragraph-down)
    
    (define-key map (kbd "S-<right>") #'mortal/select-char-right)
    (define-key map (kbd "S-<left>") #'mortal/select-char-left)

    (define-key map (kbd "S-<up>") #'mortal/select-line-up)
    (define-key map (kbd "S-<down>") #'mortal/select-line-down)


    ;; movement
    (define-key map (kbd "C-<left>") 'mortal/backward-word)
    (define-key map (kbd "C-<right>") 'mortal/forward-word)
    
    (define-key map (kbd "C-w") 'goto-line)
    
    ;; evil mode
    (define-key map (kbd "M-h") 'backward-char)
    (define-key map (kbd "M-j") 'next-line)
    (define-key map (kbd "M-k") 'previous-line)
    (define-key map (kbd "M-l") 'forward-char)

    (define-key map (kbd "M-C-h") 'mortal/backward-word)
    (define-key map (kbd "M-C-l") 'mortal/forward-word)
    

    ;; emacs movement
    (define-key map (kbd "M-p") 'previous-line)
    (define-key map (kbd "M-n") 'next-line)
    (define-key map (kbd "M-f") 'forward-char)
    (define-key map (kbd "M-b") 'backward-char)

    (define-key map (kbd "M-C-f") 'mortal/backward-word)
    (define-key map (kbd "M-C-b") 'mortal/forward-word)

    ;; move line
    (define-key map (kbd "M-<up>") 'mortal/move-line-up)    
    (define-key map (kbd "M-<down>") 'mortal/move-line-down)    

    ;; deleting
    (define-key map (kbd "C-<delete>") 'mortal/forward-delete-word)
    (define-key map (kbd "C-<backspace>") 'mortal/backward-delete-word)
    (define-key map (kbd "C-S-k") 'mortal/delete-line)

    ;; clipboard
    (define-key map (kbd "C-x") 'mortal/kill-line-or-region)
    (define-key map (kbd "C-c") 'mortal/copy-line-or-region)
    (define-key map (kbd "C-v") 'yank)

    ;; file actions
    (define-key map (kbd "C-s") 'save-buffer)
    (define-key map (kbd "C-w") 'mortal/save-and-close-file)
    (define-key map (kbd "C-n") 'mortal/new-file-buffer)
    (define-key map (kbd "C-o") 'dired)

    ;; undo / redo
    (define-key map (kbd "C-z") 'undo)
    (define-key map (kbd "C-y") 'redo)
    (define-key map (kbd "C-S-z") 'redo)

    ;; insert new line
    (define-key map (kbd "C-<return>") 'mortal/insert-line-below)
    (define-key map (kbd "C-S-<return>") 'mortal/insert-line-above)

    ;; search / replace
    ; add directory search
    (define-key map (kbd "C-f") 'mortal/isearch-forward-or-yank-region)
    (define-key map (kbd "C-r") 'mortal/query-replace-or-yank-region)
    
    map))

(provide 'mortal-keymap)
