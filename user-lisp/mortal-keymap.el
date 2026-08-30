;; -*- lexical-binding: t; -*-

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


(defun mortal/copy-line-or-region ()
  "Copy the active region, or the current line with its preceding newline."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (max (point-min) (1- (line-beginning-position)))
                         (line-end-position))
    (end-of-line)))


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
  (if (minibufferp)
      (minibuffer-keyboard-quit)
    (keyboard-quit)))


(defun mortal/tab-line-select-tab (n)
  (interactive "n")
  (when-let* ((buffer (nth (1- n) (tab-line-tabs-fixed-window-buffers))))
    (switch-to-buffer buffer)))



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

    ;; tab behaviour
    (define-key map (kbd "<backtab>") #'indent-rigidly-left-to-tab-stop)

    ;; tab management
    (define-key map (kbd "M-<left>") #'tab-line-switch-to-prev-tab)
    (define-key map (kbd "M-<right>") #'tab-line-switch-to-next-tab)
    (define-key map (kbd "C-w") #'tab-line-close-tab)

    (dotimes (i 9)
      (let ((n (1+ i)))
        (define-key esc-map (kbd (format "M-%d" n)) nil)
        (define-key map (kbd (format "M-%d" n))
                    (lambda ()
                      (interactive)
                      (mortal/tab-line-select-tab n)))))

    ;; select
    (define-key map (kbd "C-a") #'mark-whole-buffer)

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

    ;; clipboard
    (define-key map (kbd "C-x") #'mortal/kill-line-or-region)
    (define-key map (kbd "C-c") #'mortal/copy-line-or-region)
    (define-key map (kbd "C-v") #'yank)

    ;; file actions
    (define-key map (kbd "C-s") #'save-buffer)
    (define-key map (kbd "C-o") #'find-file)

    ;; undo / redo
    (define-key map (kbd "C-z") #'undo)
    (define-key map (kbd "C-y") #'undo-redo)

    ;; insert new line
    (define-key map (kbd "C-<return>") #'mortal/insert-line-below)

    ;; replace
    (define-key map (kbd "C-r") #'query-replace)


    map))

(provide 'mortal-keymap)
