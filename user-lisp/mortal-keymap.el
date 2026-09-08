;; -*- lexical-binding: t; -*-


(defun mortal/copy-line-or-region ()
  "Copy the active region, or the current line with its preceding newline."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (max (point-min) (1- (line-beginning-position)))
                         (line-end-position))
    (end-of-line)))

(require 'term)
(defun mortal/copy-or-term-interrupt ()
  "Interrupt Term, or copy the active region/current line."
  (interactive)
  (if (derived-mode-p 'term-mode)
      (term-interrupt-subjob)
    (mortal/copy-line-or-region)))




(defun mortal/move-lines-vertically (direction)
  "Move selected lines up or down and keep them selected."
  (interactive "p")
  (let* ((beg (line-beginning-position))
         (end (save-excursion
                (goto-char (region-end))
                (if (bolp) (point) (line-beginning-position 2))))
         (text (delete-and-extract-region beg end)))
    (goto-char beg)
    (forward-line direction)
    (let ((beg (point)))
      (insert text)
      (set-mark (point))
      (goto-char beg)
      (setq deactivate-mark nil))))

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
  (newline-and-indent))


(require 'delsel)
(defun mortal/quit ()
  "Quit the current operation or exit the minibuffer."
  (interactive)
  (if (active-minibuffer-window)
      (progn
        (select-window (active-minibuffer-window))
        (minibuffer-keyboard-quit))
    (keyboard-quit)))

(defun mortal/tab-line-select-tab (n)
  (interactive "n")
  (when-let* ((buffer (nth (1- n) (tab-line-tabs-fixed-window-buffers))))
    (switch-to-buffer buffer)))


(defun mortal/toggle-term ()
  (interactive)
  (if-let* ((win (get-buffer-window "*terminal*")))
      (if (eq win (selected-window))
          ;; Terminal is open and focused: close it.
          (with-selected-window win
            (let ((confirm-kill-processes nil))
              (kill-buffer-and-window)))
        ;; Terminal is open but not focused: focus it.
        (select-window win))
    ;; Terminal isn't open: create and focus it.
    (let ((win (split-window (window-main-window)
                             (- (/ (window-total-height) 3))
                             'below)))
      (select-window win)
      (term (or (getenv "SHELL") "/bin/sh"))
      (tab-line-mode -1)
      (set-process-query-on-exit-flag
       (get-buffer-process (current-buffer)) nil))))


(defun mortal/tab-line-new-tab-menu ()
  "Open the Tab Line new-tab menu."
  (interactive)
  (tab-line-new-tab (list 'mouse-1)))


(defun mortal/backward-delete-whitespace ()
  "If a region is active, delete it.  Otherwise delete whitespace
around point, crossing at most one newline: if a newline is
crossed and the previous line was blank, reindent according to
mode; if no whitespace surrounds point, delete one character the
usual way."
  (interactive "*")
  (cond
   ((use-region-p)
    (delete-region (region-beginning) (region-end)))
   ((not (looking-back "[ \t\n]" 1))
    (backward-delete-char-untabify 1))
   (t
    (let (start blank-prev)
      (save-excursion
        (skip-chars-backward " \t")
        (when (eq (char-before) ?\n)
          (backward-char)
          (skip-chars-backward " \t")
          (setq blank-prev (bolp)))
        (setq start (point)))
      (delete-region start (progn (skip-chars-forward " \t") (point)))
      (when blank-prev (indent-according-to-mode))))))


(defun mortal/forward-delete-whitespace ()
  "If a region is active, delete it.  Otherwise delete whitespace
around point, crossing at most one newline: if a newline is
crossed and the next line was blank, reindent according to
mode; if no whitespace surrounds point, delete one character the
usual way."
  (interactive "*")
  (cond
   ((use-region-p)
    (delete-region (region-beginning) (region-end)))
   ((not (looking-at "[ \t\n]"))
    (delete-char 1))
   (t
    (let (end blank-next)
      (save-excursion
        (skip-chars-forward " \t")
        (when (eq (char-after) ?\n)
          (forward-char)
          (skip-chars-forward " \t")
          (setq blank-next (eolp)))
        (setq end (point)))
      (delete-region (progn (skip-chars-backward " \t") (point)) end)
      (when blank-next (indent-according-to-mode))))))



(defun mortal/current-indent-offset ()
  "Guess the indent width for the current major mode."
  (cond ((and (boundp 'python-indent-offset) (derived-mode-p 'python-mode)) python-indent-offset)
        ((and (boundp 'c-basic-offset) (derived-mode-p 'c-mode 'c++-mode 'java-mode)) c-basic-offset)
        ((and (boundp 'js-indent-level) (derived-mode-p 'js-mode)) js-indent-level)
        ((and (boundp 'sh-basic-offset) (derived-mode-p 'sh-mode)) sh-basic-offset)
        (t tab-width)))

(defun mortal/unindent-line-or-region ()
  "Decrease indentation of the current line (or region) by one
indent step, without going past column 0."
  (interactive "*")
  (let* ((beg (if (use-region-p) (region-beginning) (line-beginning-position)))
         (end (if (use-region-p) (region-end) (line-end-position)))
         (step (if (use-region-p) (mortal/current-indent-offset)
                 (min (mortal/current-indent-offset) (current-indentation)))))
    (indent-rigidly beg end (- step))))



(defun mortal/newline-and-indent-current ()
  "Insert a newline without ever reindenting the previous line.
Indent only the newly created current line.
If in the minibuffer, just run whatever command RET is normally
bound to there instead.
If in a terminal/REPL-like buffer (comint, eshell, term), send
the current input instead of inserting a newline."
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete-and-exit))
   ((derived-mode-p 'term-mode)
    (term-send-input))
   (t
    (let (electric-indent-mode)      ; temporarily disable electric-indent's
      (newline))                     ; hooks for this one newline
    (indent-according-to-mode))))    ; indent just the line we landed on


;; hack for marking whole buffer without moving point, because that would move view

(require 'cl-lib)

(defvar mortal/tsr--overlay nil)

(defun mortal/tsr--cleanup-overlay ()
  (when (overlayp mortal/tsr--overlay)
    (delete-overlay mortal/tsr--overlay))
  (setq mortal/tsr--overlay nil))

(defun mortal/tsr--pre-command ()
  "Run the next command as if the whole buffer were the active region,
without ever moving point, mark, or scrolling the window."
  (remove-hook 'pre-command-hook #'mortal/tsr--pre-command t)
  (mortal/tsr--cleanup-overlay)
  (cl-letf (((symbol-function 'region-beginning) (lambda () (point-min)))
            ((symbol-function 'region-end)       (lambda () (point-max)))
            ((symbol-function 'use-region-p)     (lambda () t))
            ((symbol-function 'region-active-p)  (lambda () t))
            (mark-active t))
    (call-interactively this-command))
  (setq this-command 'ignore))

(defun mortal/temp-select-all-dispatch ()
  "Visually mark the whole buffer with an overlay and arrange for the
next command to act on it as the region -- all without moving point,
mark, or scrolling the window."
  (interactive)
  (mortal/tsr--cleanup-overlay)
  (setq mortal/tsr--overlay (make-overlay (point-min) (point-max)))
  (overlay-put mortal/tsr--overlay 'face 'region)
  (overlay-put mortal/tsr--overlay 'priority 1000)
  (add-hook 'pre-command-hook #'mortal/tsr--pre-command nil t))




(require 'tab-line)

(defvar mortal-map
  (let ((map (make-sparse-keymap)))
    ;; undefine C-*, M-*, and C-M-* besides C-i, C-j, C-m.
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
    (define-key map (kbd "M-x") #'undefined)
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

    ;; better deletion
    (define-key map (kbd "<backspace>") #'mortal/backward-delete-whitespace)
    (define-key map (kbd "<delete>") #'mortal/forward-delete-whitespace)

    ;; indent behaviour
    (define-key map (kbd "<backtab>") #'mortal/unindent-line-or-region)
    (define-key map (kbd "RET") #'mortal/newline-and-indent-current)
    
    ;; emacs prefixes
    (define-key map (kbd "<f1>") ctl-x-map)
    (define-key map (kbd "<f2>") help-map)
    (define-key map (kbd "M-x") #'execute-extended-command)
    (define-key map (kbd "C-g") goto-map)
    (define-key map (kbd "C-f") search-map)

    ;; which key binds
    (dolist (entry (accessible-keymaps global-map))
      (let ((map (cdr entry)))
        (unless (lookup-key map (kbd "<next>"))
          (define-key map (kbd "<next>") #'which-key-show-next-page-cycle))
        (unless (lookup-key map (kbd "<prior>"))
          (define-key map (kbd "<prior>") #'which-key-show-previous-page-cycle))))

    ;; tab management
    (define-key map (kbd "M-<left>") #'tab-line-switch-to-prev-tab)
    (define-key map (kbd "M-<right>") #'tab-line-switch-to-next-tab)
    (define-key map (kbd "C-w") #'tab-line-close-tab)
    (define-key map (kbd "C-b") #'speedbar)

    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key esc-map (kbd (format "M-%d" n)) nil)
        (define-key map (kbd (format "M-%d" n))
                    (lambda ()
                      (interactive)
                      (mortal/tab-line-select-tab n)))))

    ;; select
    (define-key map (kbd "C-a") #'mortal/temp-select-all-dispatch)

    ;; emacs movement
    (define-key map (kbd "M-p") #'previous-line)
    (define-key map (kbd "M-n") #'next-line)
    (define-key map (kbd "M-f") #'forward-char)
    (define-key map (kbd "M-b") #'backward-char)

    ;; move line
    (define-key map (kbd "M-<up>") #'mortal/move-line-up)
    (define-key map (kbd "M-<down>") #'mortal/move-line-down)

    ;; deleting
    (define-key map (kbd "C-k") #'mortal/delete-line)

    ;; term
    (define-key map (kbd "C-M-t") #'mortal/toggle-term)
    (define-key map (kbd "C-S-c") #'mortal/copy-line-or-region)
    
    ;; clipboard
    (define-key map (kbd "C-x") #'mortal/kill-line-or-region)
    (define-key map (kbd "C-c") #'mortal/copy-or-term-interrupt)
    (define-key map (kbd "C-v") #'yank)

    ;; file actions
    (define-key map (kbd "C-s") #'save-buffer)
    (define-key map (kbd "C-o") #'find-file)
    (define-key map (kbd "C-n") #'mortal/tab-line-new-tab-menu)

    ;; undo / redo
    (define-key map (kbd "C-z") #'undo)
    (define-key map (kbd "C-y") #'undo-redo)

    ;; insert new line
    (define-key map (kbd "C-<return>") #'mortal/insert-line-below)

    ;; replace
    (define-key map (kbd "C-r") #'query-replace)
    
    map))


(provide 'mortal-keymap)
