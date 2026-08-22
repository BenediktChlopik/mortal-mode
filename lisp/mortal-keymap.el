;; -*- Lexical-binding: t; -*-

(defun mortal/quit ()
  "Quit the current operation or exit the minibuffer."
  (interactive)
  (if (minibufferp)
      (minibuffer-keyboard-quit)
    (keyboard-quit)))


(defun mortal/new-tab-with-empty-buffer ()
  "Create a new tab-bar tab and switch it to a fresh empty buffer."
  (interactive)
  (tab-bar-new-tab)
  (switch-to-buffer (generate-new-buffer "*new*"))
  (text-mode))



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


(defun mortal/select-paragraph-up ()
  "Start the region if none is active, then extend it backward by one paragraph."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (backward-paragraph 1))

(defun mortal/select-paragraph-down ()
  "Start the region if none is active, then extend it forward by one paragraph."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (forward-paragraph 1))


(defun mortal/select-char-left ()
  "Start the region if none is active, then extend it backward by one char."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (backward-char 1))

(defun mortal/select-char-right ()
  "Start the region if none is active, then extend it forward by one char."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (forward-char 1))


(defun mortal/select-line-up ()
  "Start the region if none is active, then extend it up by one line."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (let ((col (current-column)))
    (forward-line -1)
    (move-to-column col)))

(defun mortal/select-line-down ()
  "Start the region if none is active, then extend it down by one line."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (let ((col (current-column)))
    (forward-line 1)
    (move-to-column col)))



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



(defun mortal/save-and-close-file ()
  "Save the current buffer, and close the window."
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (delete-window))

(defun mortal/copy-line-or-region ()
  "Copy the active region to the kill ring, or the whole current
line if no region is active. Point is moved to the start of the
line afterward."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (line-beginning-position) (line-end-position)))
  (beginning-of-line))



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
    (isearch-complete)))

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



(defun mortal/copy-line-down ()
  "Duplicate the current line, placing the copy below and moving point there."
  (interactive)
  (let ((col (current-column))
        (line (buffer-substring (line-beginning-position) (line-end-position))))
    (end-of-line)
    (newline)
    (insert line)
    (move-to-column col)))

(defun mortal/copy-line-up ()
  "Duplicate the current line, placing the copy above and moving point there."
  (interactive)
  (let ((col (current-column))
        (line (buffer-substring (line-beginning-position) (line-end-position))))
    (beginning-of-line)
    (insert line "\n")
    (forward-line -1)
    (move-to-column col)))

(defun mortal/close-current-tab ()
  "Close the current tab. If it's the last tab, delete the frame instead."
  (interactive)
  (if (> (length (tab-bar-tabs)) 1)
      (tab-bar-close-tab)
    (delete-frame)))



(defvar mortal-map
  (let ((map (make-sparse-keymap)))
    ;; undefine C-*, M-*, and C-M-* besides M-x, C-i, C-j, C-m.
    (define-key map (kbd "C-a") #'undefined)
    (define-key map (kbd "C-b") #'undefined)
    (define-key map (kbd "C-c") #'undefined)
    (define-key map (kbd "C-d") #'undefined)
    (define-key map (kbd "C-e") #'undefined)
    (define-key map (kbd "C-f") #'undefined)
    (define-key map (kbd "C-g") #'undefined)
    (define-key map (kbd "C-h") #'undefined)
    (define-key map (kbd "C-k") #'undefined)
    (define-key map (kbd "C-l") #'undefined)
    (define-key map (kbd "C-n") #'undefined)
    (define-key map (kbd "C-o") #'undefined)
    (define-key map (kbd "C-p") #'undefined)
    (define-key map (kbd "C-q") #'undefined)
    (define-key map (kbd "C-r") #'undefined)
    (define-key map (kbd "C-s") #'undefined)
    (define-key map (kbd "C-t") #'undefined)
    (define-key map (kbd "C-u") #'undefined)
    (define-key map (kbd "C-v") #'undefined)
    (define-key map (kbd "C-w") #'undefined)
    (define-key map (kbd "C-x") #'undefined)
    (define-key map (kbd "C-y") #'undefined)
    (define-key map (kbd "C-z") #'undefined)

    (define-key map (kbd "M-a") #'undefined)
    (define-key map (kbd "M-b") #'undefined)
    (define-key map (kbd "M-c") #'undefined)
    (define-key map (kbd "M-d") #'undefined)
    (define-key map (kbd "M-e") #'undefined)
    (define-key map (kbd "M-f") #'undefined)
    (define-key map (kbd "M-g") #'undefined)
    (define-key map (kbd "M-h") #'undefined)
    (define-key map (kbd "M-i") #'undefined)
    (define-key map (kbd "M-j") #'undefined)
    (define-key map (kbd "M-k") #'undefined)
    (define-key map (kbd "M-l") #'undefined)
    (define-key map (kbd "M-m") #'undefined)
    (define-key map (kbd "M-n") #'undefined)
    (define-key map (kbd "M-o") #'undefined)
    (define-key map (kbd "M-p") #'undefined)
    (define-key map (kbd "M-q") #'undefined)
    (define-key map (kbd "M-r") #'undefined)
    (define-key map (kbd "M-s") #'undefined)
    (define-key map (kbd "M-t") #'undefined)
    (define-key map (kbd "M-u") #'undefined)
    (define-key map (kbd "M-v") #'undefined)
    (define-key map (kbd "M-w") #'undefined)
    (define-key map (kbd "M-y") #'undefined)
    (define-key map (kbd "M-z") #'undefined)

    (define-key map (kbd "C-M-a") #'undefined)
    (define-key map (kbd "C-M-b") #'undefined)
    (define-key map (kbd "C-M-c") #'undefined)
    (define-key map (kbd "C-M-d") #'undefined)
    (define-key map (kbd "C-M-e") #'undefined)
    (define-key map (kbd "C-M-f") #'undefined)
    (define-key map (kbd "C-M-g") #'undefined)
    (define-key map (kbd "C-M-h") #'undefined)
    (define-key map (kbd "C-M-i") #'undefined)
    (define-key map (kbd "C-M-j") #'undefined)
    (define-key map (kbd "C-M-k") #'undefined)
    (define-key map (kbd "C-M-l") #'undefined)
    (define-key map (kbd "C-M-m") #'undefined)
    (define-key map (kbd "C-M-n") #'undefined)
    (define-key map (kbd "C-M-o") #'undefined)
    (define-key map (kbd "C-M-p") #'undefined)
    (define-key map (kbd "C-M-q") #'undefined)
    (define-key map (kbd "C-M-r") #'undefined)
    (define-key map (kbd "C-M-s") #'undefined)
    (define-key map (kbd "C-M-t") #'undefined)
    (define-key map (kbd "C-M-u") #'undefined)
    (define-key map (kbd "C-M-v") #'undefined)
    (define-key map (kbd "C-M-w") #'undefined)
    (define-key map (kbd "C-M-x") #'undefined)
    (define-key map (kbd "C-M-y") #'undefined)
    (define-key map (kbd "C-M-z") #'undefined)

    
    (define-key map (kbd "<escape>") #'mortal/quit)
    
    ;; emacs prefixes
    (define-key map (kbd "<f1>") ctl-x-map)
    (define-key map (kbd "<f3>") ctl-x-r-map)
    (define-key map (kbd "<f5>") #'help-command)

    ;; window management
    (define-key map (kbd "<f2> 1") #'delete-other-windows)
    (define-key map (kbd "<f2> 2") #'split-window-right)
    (define-key map (kbd "<f2> 3") #'split-window-below)
    
    (define-key map (kbd "<f2> <left>") #'windmove-left)
    (define-key map (kbd "<f2> <right>") #'windmove-right)
    (define-key map (kbd "<f2> <up>") #'windmove-up)
    (define-key map (kbd "<f2> <down>") #'windmove-down)

    ;; tab management 
    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key map
                    (kbd (format "C-%d" n))
                    (eval `(lambda ()
                             (interactive)
                             (tab-bar-select-tab ,n))))))
    
    (define-key map (kbd "C-S-n") #'mortal/new-tab-with-empty-buffer)
    (define-key map (kbd "C-w") #'mortal/close-current-tab)
    
    ;; select
    (define-key map (kbd "C-a") #'mark-whole-buffer)
    
    (define-key map (kbd "C-S-<left>") #'mortal/select-word-left)
    (define-key map (kbd "C-S-<right>") #'mortal/select-word-right)
    (define-key map (kbd "C-S-<up>") #'mortal/select-paragraph-up)
    (define-key map (kbd "C-S-<down>") #'mortal/select-paragraph-down)
    
    (define-key map (kbd "S-<left>") #'mortal/select-char-left)
    (define-key map (kbd "S-<right>") #'mortal/select-char-right)

    (define-key map (kbd "S-<up>") #'mortal/select-line-up)
    (define-key map (kbd "S-<down>") #'mortal/select-line-down)


    ;; movement
    (define-key map (kbd "C-<right>") #'mortal/forward-word)
    (define-key map (kbd "C-<left>") #'mortal/backward-word)
    
    (define-key map (kbd "C-g") #'goto-line)
    
    ;; evil mode
    (define-key map (kbd "M-h") #'backward-char)
    (define-key map (kbd "M-j") #'next-line)
    (define-key map (kbd "M-k") #'previous-line)
    (define-key map (kbd "M-l") #'forward-char)

    (define-key map (kbd "M-C-l") #'mortal/forward-word)
    (define-key map (kbd "M-C-h") #'mortal/backward-word)
    (define-key map (kbd "M-C-j") #'forward-paragraph)
    (define-key map (kbd "M-C-k") #'backward-paragraph)

    ;; emacs movement
    (define-key map (kbd "M-p") #'previous-line)
    (define-key map (kbd "M-n") #'next-line)
    (define-key map (kbd "M-f") #'forward-char)
    (define-key map (kbd "M-b") #'backward-char)

    (define-key map (kbd "M-C-f") #'mortal/forward-word)
    (define-key map (kbd "M-C-b") #'mortal/backward-word)
    (define-key map (kbd "M-C-n") #'forward-paragraph)
    (define-key map (kbd "M-C-p") #'backward-paragraph)


    ;; move line
    (define-key map (kbd "M-<up>") #'mortal/move-line-up)    
    (define-key map (kbd "M-<down>") #'mortal/move-line-down)

    ;; copy line
    (define-key map (kbd "C-M-S-<down>") #'mortal/copy-line-down)    
    (define-key map (kbd "C-M-S-<up>") #'mortal/copy-line-up)    

    ;; deleting
    (define-key map (kbd "C-<delete>") #'mortal/forward-delete-word)
    (define-key map (kbd "C-<backspace>") #'mortal/backward-delete-word)
    (define-key map (kbd "C-S-k") #'mortal/delete-line)

    ;; clipboard
    (define-key map (kbd "C-x") #'mortal/kill-line-or-region)
    (define-key map (kbd "C-c") #'mortal/copy-line-or-region)
    (define-key map (kbd "C-v") #'yank)

    ;; file actions
    (define-key map (kbd "C-s") #'save-buffer)
    (define-key map (kbd "C-w") #'mortal/save-and-close-file)
    (define-key map (kbd "C-n") #'mortal/new-file-buffer)
    (define-key map (kbd "C-o") #'find-file)

    ;; undo / redo
    (define-key map (kbd "C-z") #'undo)
    (define-key map (kbd "C-y") #'undo-redo)

    ;; insert new line
    (define-key map (kbd "C-<return>") #'mortal/insert-line-below)
    (define-key map (kbd "C-S-<return>") #'mortal/insert-line-above)

    ;; search / replace
    (define-key map (kbd "C-f") #'mortal/isearch-forward-or-yank-region)
    (define-key map (kbd "C-r") #'mortal/query-replace-or-yank-region)
    
    map))

(provide 'mortal-keymap)
