;; -*- lexical-binding: t; -*-

(defun mortal/kill-region-and-yank ()
  "If a region is active, delete it, then yank the kill-ring text."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (yank))


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


(defun mortal/forward-word ()
  "Skip over whitespace. If there was none to skip, move past the word."
  (interactive)
  (forward-same-syntax)
  (skip-chars-forward " \t\r"))

(defun mortal/backward-word ()
  "Skip over whitespace. If there was none to skip, move past the word."
  (interactive)
  (forward-same-syntax -1)
  (skip-chars-backward " \t\r"))


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

(defun mortal/kill-region-if-active ()
  "Kill the region, but only if it is actually active.
Unlike `kill-region' called interactively, this refuses to act
on a stale/inactive mark."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))))


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
    (define-key map (kbd "<f2> <right>") 'other-right)
    (define-key map (kbd "<f2> <up>") 'windmove-up)
    (define-key map (kbd "<f2> <down>") 'windmove-down)

    ;; select
    (define-key map (kbd "C-a") 'mark-whole-buffer)

    ;; movement
    (define-key map (kbd "C-<left>") 'mortal/backward-word)
    (define-key map (kbd "C-<right>") 'mortal/forward-word)

    ;; move line
    (define-key map (kbd "M-<up>") 'mortal/move-line-up)    
    (define-key map (kbd "M-<down>") 'mortal/move-line-down)    

    ;; deleting
    (define-key map (kbd "C-<delete>") 'mortal/forward-delete-word)
    (define-key map (kbd "C-<backspace>") 'mortal/backward-delete-word)
    (define-key map (kbd "C-S-k") 'mortal/delete-line)

    ;; clipboard
    (define-key map (kbd "C-x") 'mortal/kill-region-if-active)
    (define-key map (kbd "C-c") 'kill-ring-save)
    (define-key map (kbd "C-v") 'mortal/kill-region-and-yank)

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

    ;; tab management
    ; C-<tab> already bound to switching between tabs
    (dotimes (i 9)
      (let ((n (1+ i)))
	(define-key map (kbd (format "<f2>-%d" n))
			(lambda () (interactive) (tab-bar-select-tab n)))))

    ;; search / replace
    ; search should feel like in kate, add directory search
    (define-key map (kbd "C-f") 'isearch-forward)
    (define-key map (kbd "C-r") 'query-replace)

    ;; evil mode
    (define-key map (kbd "M-h") 'backward-char)
    (define-key map (kbd "M-j") 'next-line)
    (define-key map (kbd "M-k") 'previous-line)
    (define-key map (kbd "M-l") 'forward-char)

    ;; emacs movement
    (define-key map (kbd "M-p") 'previous-line)
    (define-key map (kbd "M-n") 'next-line)
    (define-key map (kbd "M-f") 'forward-char)
    (define-key map (kbd "M-b") 'backward-char)
    
    map))

(provide 'mortal-keymap)
