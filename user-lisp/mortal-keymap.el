;; -*- lexical-binding: t; -*-


(defun mortal/move-line-up ()
  "Move the current line up and keep it selected if it was.
If a region is active, move all marked lines up instead."
  (interactive)
  (if (not (use-region-p))
      (progn (transpose-lines 1) (forward-line -2))
    (let* ((beg (save-excursion (goto-char (region-beginning)) (line-beginning-position)))
           (end (save-excursion (goto-char (region-end))
                                (if (bolp) (point) (line-beginning-position 2))))
           (prev-beg (save-excursion (goto-char beg) (forward-line -1) (point))))
      (if (= prev-beg beg)
          (message "Can't move further up")
        (let ((above (buffer-substring prev-beg beg))
              (region (buffer-substring beg end)))
          (atomic-change-group
            (delete-region prev-beg end)
            (goto-char prev-beg)
            (insert region above))
          (set-mark (+ prev-beg (length region)))
          (goto-char prev-beg)
          (setq deactivate-mark nil))))))

(defun mortal/move-line-down ()
  "Move the current line down and keep it selected if it was.
If a region is active, move all marked lines down instead."
  (interactive)
  (if (not (use-region-p))
      (progn (forward-line 1) (transpose-lines 1) (forward-line -1))
    (let* ((beg (save-excursion (goto-char (region-beginning)) (line-beginning-position)))
           (end (save-excursion (goto-char (region-end))
                                (if (bolp) (point) (line-beginning-position 2))))
           (next-end (save-excursion (goto-char end) (forward-line 1) (point))))
      (if (= next-end end)
          (message "Can't move further down")
        (let ((region (buffer-substring beg end))
              (below (buffer-substring end next-end)))
          (atomic-change-group
            (delete-region beg next-end)
            (goto-char beg)
            (insert below region))
          (goto-char (+ beg (length below) (length region)))
          (set-mark (- (point) (length region)))
          (setq deactivate-mark nil))))))



(defun mortal/delete-line ()
  "Delete the current line, including its trailing newline."
  (interactive)
  (delete-region (line-beginning-position) (1+ (line-end-position))))


(defun mortal/insert-line-below ()
  "Insert a new line below the current line and move point there."
  (interactive)
  (end-of-line)
  (newline-and-indent))


(defun mortal/copy-line-or-region ()
  "Copy the active region, or the current line with its preceding newline."
  (interactive)
  (if (use-region-p)
      (copy-region-as-kill (region-beginning) (region-end))
    (copy-region-as-kill (max (point-min) (1- (line-beginning-position)))
                         (line-end-position))
    (end-of-line)))

(defun mortal/kill-line-or-region ()
  "Kill the active region, or the whole current line if no region is active."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))


(require 'delsel)
(defun mortal/quit ()
  "Do the right thing when quitting, mimicking `keyboard-quit' contextually.

Priority mirrors how a real C-g would be dispatched by the active keymap:
1. If the current local keymap has its own binding for C-g (other than
   this command), use that directly — e.g. `isearch-abort' in isearch,
   or whatever a minibuffer/completion framework binds locally.
2. Inside a minibuffer with no such local binding, quit it (handles a
   stray active region there too).
3. With an active region, just deactivate the mark instead of signalling
   quit.
4. Inside a recursive edit, exit it.
5. Otherwise, fall back to plain `keyboard-quit'."
  (interactive)
  (let ((cmd (lookup-key (current-local-map) (kbd "C-g"))))
    (cond
     ((and (commandp cmd)
           (not (eq cmd 'mortal/quit)))
      (call-interactively cmd))
     ((> (minibuffer-depth) 0)
      (minibuffer-keyboard-quit))
     ((region-active-p)
      (when (boundp 'saved-region-selection)
        (setq saved-region-selection nil))
      (let (select-active-regions)
        (deactivate-mark)))
     ((> (recursion-depth) 0)
      (exit-recursive-edit))
     (t (keyboard-quit)))))

(defun mortal/tab-line-select-tab (n)
  (interactive "n")
  (when-let* ((buffer (nth (1- n) (tab-line-tabs-fixed-window-buffers))))
    (switch-to-buffer buffer)))

(defun mortal/tab-line-new-tab-menu ()
  "Open the Tab Line new-tab menu."
  (interactive)
  (tab-line-new-tab (list 'mouse-1)))



(defun mortal/move (dir)
  "Move point one \"smart\" step in DIR (1 = forward, -1 = backward).

Rules (stated for DIR = 1; mirror for DIR = -1):

1. Every character belongs to one of three groups: word characters
   ([A-Za-z0-9]), special characters (everything else except
   tabs/spaces/newline), and whitespace (tabs/spaces).

2. If point is right before a character string (i.e. not sitting on
   whitespace), a step always crosses two of those three groups: it
   skips over the run of the group at point, and then continues and
   skips over the following run too, even if that run belongs to a
   different group (e.g. word characters into special characters, or
   the reverse). It does NOT stop merely because the character class
   changed.

3. While crossing that second group, do NOT cross a newline: if the
   second group is whitespace and it is the line's trailing
   whitespace (i.e. crossing it would put point at the end of the
   line), stop right before it instead -- crossing only the first
   group.

4. If point is already at such a stop (only tabs/spaces, or nothing,
   between point and the end of the line), then this call crosses
   the newline and lands at the tab indent of the next line."
  (let ((skip (lambda (chars)
                (if (> dir 0) (skip-chars-forward chars)
                  (skip-chars-backward chars))))
        (at-edge-p (lambda ()
                     (if (> dir 0)
                         (looking-at "[ \t]*$")
                       (looking-back "^[ \t]*" (line-beginning-position)))))
        (on-ws-p (lambda ()
                   (if (> dir 0)
                       (looking-at "[ \t]")
                     (looking-back "[ \t]" (1- (point))))))
        (class-at (lambda ()
                    ;; Class of the character adjacent to point in DIR:
                    ;; 'word, 'special, 'ws, or nil (newline / buffer edge).
                    (let ((ch (if (> dir 0) (char-after) (char-before))))
                      (cond
                       ((null ch) nil)
                       ((eq ch ?\n) nil)
                       ((string-match-p "[A-Za-z0-9]" (string ch)) 'word)
                       ((string-match-p "[ \t]" (string ch)) 'ws)
                       (t 'special)))))
        (class-chars (lambda (class)
                       (pcase class
                         ('word "A-Za-z0-9")
                         ('ws " \t")
                         ('special "^A-Za-z0-9 \t\n")))))
    (cond
     ;; Rule 4: already at a stop -> cross the newline.
     ((funcall at-edge-p)
      (forward-line dir)
      (if (> dir 0)
          (skip-chars-forward " \t")
        (end-of-line)
        (skip-chars-backward " \t")))
     ;; Sitting on non-trailing whitespace -> just cross it.
     ((funcall on-ws-p)
      (funcall skip " \t"))
     ;; Rules 1-3: cross two of the three groups.
     (t
      (let ((c1 (funcall class-at)))
        (funcall skip (funcall class-chars c1))
        (let ((c2 (funcall class-at)))
          (when (and c2 (not (funcall at-edge-p)))
            (funcall skip (funcall class-chars c2)))))))))

(defun mortal/forward ()
  "Move point forward one \"smart\" step."
  (interactive)
  (mortal/move 1))

(defun mortal/backward ()
  "Move point backward one \"smart\" step."
  (interactive)
  (mortal/move -1))

(defun mortal/mark-n-forward ()
  "Move point forward one \"smart\" step, starting a mark region if none is active."
  (interactive)
  (unless (region-active-p)
    (push-mark (point) t t))
  (mortal/move 1))

(defun mortal/mark-n-backward ()
  "Move point backward one \"smart\" step, starting a mark region if none is active."
  (interactive)
  (unless (region-active-p)
    (push-mark (point) t t))
  (mortal/move -1))



(defun mortal/forward-delete-whitespace ()
  "If a region is active, delete it.  Otherwise delete whitespace
after point, crossing at most one newline: if a newline is
crossed and the next line was blank, reindent according to
mode; if no whitespace follows point, delete one character the
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
      (delete-region (point) end)
      (when blank-next (indent-according-to-mode))))))


(defun mortal/backward-delete-whitespace ()
  "If a region is active, delete it.  Otherwise delete whitespace
before point, crossing at most one newline: if a newline is
crossed and the previous line was blank, reindent according to
mode; if no whitespace precedes point, delete one character the
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
      (delete-region start (point))
      (when blank-prev (indent-according-to-mode))))))



(defun mortal/current-indent-offset ()
  "Guess the indent width for the current major mode."
  (cond
   ((and (boundp 'python-indent-offset) (derived-mode-p 'python-mode 'python-ts-mode))
    python-indent-offset)
   ((and (boundp 'c-basic-offset) (derived-mode-p 'c-mode 'c++-mode 'java-mode))
    c-basic-offset)
   ((and (boundp 'c-ts-mode-indent-offset) (derived-mode-p 'c-ts-mode 'c++-ts-mode))
    c-ts-mode-indent-offset)
   ((and (boundp 'java-ts-mode-indent-offset) (derived-mode-p 'java-ts-mode))
    java-ts-mode-indent-offset)
   ((and (boundp 'js-indent-level) (derived-mode-p 'js-mode 'js-ts-mode 'json-mode))
    js-indent-level)
   ((and (boundp 'typescript-indent-level) (derived-mode-p 'typescript-mode))
    typescript-indent-level)
   ((and (boundp 'typescript-ts-mode-indent-offset) (derived-mode-p 'typescript-ts-mode 'tsx-ts-mode))
    typescript-ts-mode-indent-offset)
   ((and (boundp 'json-ts-mode-indent-offset) (derived-mode-p 'json-ts-mode))
    json-ts-mode-indent-offset)
   ((and (boundp 'css-indent-offset) (derived-mode-p 'css-mode))
    css-indent-offset)
   ((and (boundp 'css-ts-mode-indent-offset) (derived-mode-p 'css-ts-mode))
    css-ts-mode-indent-offset)
   ((and (boundp 'rust-indent-offset) (derived-mode-p 'rust-mode))
    rust-indent-offset)
   ((and (boundp 'rust-ts-mode-indent-offset) (derived-mode-p 'rust-ts-mode))
    rust-ts-mode-indent-offset)
   ((and (boundp 'sh-basic-offset) (derived-mode-p 'sh-mode))
    sh-basic-offset)
   (t tab-width)))


(defun mortal/unindent-line-or-region ()
  "Decrease indentation of the current line (or region) by one
indent step, without going past column 0. If a region is active,
it is expanded to cover whole lines, and stays selected as such."
  (interactive "*")
  (let* ((region-p (use-region-p))
         (beg (if region-p (save-excursion (goto-char (region-beginning)) (line-beginning-position))
                (line-beginning-position)))
         (end (if region-p (save-excursion (goto-char (region-end)) (line-end-position))
                (line-end-position)))
         (offset (mortal/current-indent-offset))
         (end-marker (copy-marker end))
         (deactivate-mark nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) end-marker)
        (indent-line-to (max 0 (- (current-indentation) offset)))
        (forward-line 1)))
    (when region-p
      (goto-char beg)
      (push-mark end-marker nil t))
    (set-marker end-marker nil)))


(require 'esh-mode)
(defun mortal/newline-and-indent-current ()
  "Insert a newline without ever reindenting the previous line.
Indent only the newly created current line.

If in isearch, exit the search.
If in the minibuffer, check what RET is bound to in the current
local keymap (e.g. a completion framework's map) and use that;
otherwise fall back to complete-and-exit/exit-minibuffer.
Otherwise, insert a newline without reindenting the previous line,
indenting only the newly created line."
  (interactive)
  (cond
   (isearch-mode
    (isearch-exit))
   ((minibufferp)
    (let ((cmd (lookup-key (current-local-map) (kbd "RET"))))
      (if (and (commandp cmd)
               (not (eq cmd 'mortal/newline-and-indent-current)))
          (call-interactively cmd)(if minibuffer-completion-table
                                      (minibuffer-complete-and-exit)
                                    (exit-minibuffer)))))
   (t
    (let (electric-indent-mode)      ; temporarily disable electric-indent's
      (newline))                     ; hooks for this one newline
    (indent-according-to-mode))))    ; indent just the line we landed on



;; hack for marking whole buffer without moving point, because that would move view

(require 'cl-lib)

(defun mortal/temp-select-all-dispatch ()
  "Visually mark the whole buffer and make the next command act on it.
Point, mark, and window scrolling are left unchanged."
  (interactive)
  (let* ((overlay (make-overlay (point-min) (point-max)))
         (next-command-fn nil))
    (overlay-put overlay 'face 'region)
    (overlay-put overlay 'priority 1000)
    
    (setq next-command-fn
          (lambda ()
            (remove-hook 'pre-command-hook next-command-fn t)
            (delete-overlay overlay)

            (cl-letf (((symbol-function 'region-beginning)
                       (lambda () (point-min)))
                      ((symbol-function 'region-end)
                       (lambda () (point-max)))
                      ((symbol-function 'use-region-p)
                       (lambda () t))
                      ((symbol-function 'region-active-p)
                       (lambda () t))
                      (mark-active t))
              (call-interactively this-command))
            
            ;; Prevent Emacs from executing the command a second time.
            (setq this-command 'ignore)))
    
    (add-hook 'pre-command-hook next-command-fn nil t)))


(defun mortal/undo ()
  "Call `undo', ignoring any active region.
Emacs's `undo' restricts itself to changes within the region
when one is active (`undo-in-region'); this deactivates the
mark first so undo always applies to the whole buffer."
  (interactive)
  (when (use-region-p)
    (deactivate-mark))
  (undo))

(defun mortal/redo ()
  "Call `undo-redo', ignoring any active region.
Mirrors `mortal/undo': deactivates the mark first so redo always
applies to the whole buffer instead of being restricted by an
active region."
  (interactive)
  (when (use-region-p)
    (deactivate-mark))
  (undo-redo))



(defun mortal/comment-dwim ()
  "Like `comment-dwim', but expand a half-marked region to whole lines first,
and keep the region active/marked afterwards. If nothing is marked, mark
the current line first."
  (interactive "*")
  (if (and (use-region-p)
           (not (eq (region-beginning) (region-end))))
      (let ((beg (save-excursion
                   (goto-char (region-beginning))
                   (line-beginning-position)))
            (end (save-excursion
                   (goto-char (region-end))
                   (if (bolp) (point) (line-end-position)))))
        (goto-char beg)
        (push-mark end nil t)
        (comment-dwim nil)
        ;; comment-dwim/comment-region typically deactivate the mark;
        ;; force it back on with the (possibly shifted) bounds.
        (setq deactivate-mark nil)
        (activate-mark))
    (let ((beg (line-beginning-position))
          (end (line-end-position)))
      (goto-char beg)
      (push-mark end nil t)
      (comment-dwim nil)
      (setq deactivate-mark nil)
      (activate-mark))))

(defun mortal/yank-plain ()
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end)))
  (insert (substring-no-properties (current-kill 0)) "\n"))


(require 'tab-line)

(defvar mortal-map
  (let ((map (make-sparse-keymap)))
    ;; undefine C-*, M-*, and C-M-* besides C-i, C-m.
    (define-key map (kbd "C-a") #'undefined)
    (define-key map (kbd "C-b") #'undefined)
    (define-key map (kbd "C-c") #'undefined)
    (define-key map (kbd "C-d") #'undefined)
    (define-key map (kbd "C-e") #'undefined)
    (define-key map (kbd "C-f") #'undefined)
    (define-key map (kbd "C-g") #'undefined)
    (define-key map (kbd "C-h") #'undefined)
    (define-key map (kbd "C-j") #'undefined)
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
    
    
    ;; quiting
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

    
    ;; clipboard
    (define-key map (kbd "C-x") #'mortal/kill-line-or-region)
    (define-key map (kbd "C-c") #'mortal/copy-line-or-region)
    (define-key map (kbd "C-v") #'mortal/yank-plain)

    ;; file actions
    (define-key map (kbd "C-s") #'save-buffer)
    (define-key map (kbd "C-o") #'find-file)
    (define-key map (kbd "C-n") #'mortal/tab-line-new-tab-menu)

    ;; undo / redo
    (define-key map (kbd "C-z") #'mortal/undo)
    (define-key map (kbd "C-y") #'mortal/redo)

    ;; insert new line
    (define-key map (kbd "C-<return>") #'mortal/insert-line-below)

    ;; replace
    (define-key map (kbd "C-r") #'query-replace)
    
    ;; auto commenting
    (define-key map (kbd "C-;") #'mortal/comment-dwim)
    
    ;; smarter point movement
    (define-key map (kbd "C-<left>") #'mortal/backward)
    (define-key map (kbd "C-<right>") #'mortal/forward)
    
    (define-key map (kbd "C-S-<left>") #'mortal/mark-n-backward)
    (define-key map (kbd "C-S-<right>") #'mortal/mark-n-forward)

    
    map))


(provide 'mortal-keymap)
