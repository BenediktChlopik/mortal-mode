;; -*- lexical-binding: t; -*-

;;; ---------------------------------------------------------------------------
;;; Dependencies
;;; ---------------------------------------------------------------------------

(require 'cl-lib)
(require 'delsel)
(require 'esh-mode)
(require 'tab-line)
(require 'completion-preview)

(require 'mortal-move)


;;; ---------------------------------------------------------------------------
;;; Line editing
;;; ---------------------------------------------------------------------------

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
  "Delete whole lines covered by the active region, or the current line."
  (interactive)
  (if (use-region-p)
      (delete-region
       (save-excursion
         (goto-char (region-beginning))
         (line-beginning-position))
       (save-excursion
         (goto-char (region-end))
         (if (= (region-end) (line-beginning-position))
             (point)
           (line-beginning-position 2))))
    (delete-region (line-beginning-position)
                   (line-beginning-position 2))))

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

;;; ---------------------------------------------------------------------------
;;; Quitting
;;; ---------------------------------------------------------------------------

(defun mortal/quit ()
  "Do the right thing when quitting, mimicking `keyboard-quit' contextually.

Priority mirrors how a real C-g would actually be dispatched by Emacs:
1. Walk the active keymaps in the same precedence Emacs itself uses for
   key lookup (`overriding-terminal-local-map', `overriding-local-map',
   emulation-mode-map-alists, minor-mode-overriding-map-alist,
   minor-mode-map-alist, then the local/major-mode map) and, if any of
   them (other than the global map) binds C-g to something other than
   this command, run that directly.  This is what correctly catches
   things like `isearch-abort' or a transient/repeat map's own quit
   command, which never live in the local map.
2. Inside a minibuffer with no such binding, quit it (handles a stray
   active region there too, and pops one level for nested minibuffers).
3. With an active region, just deactivate the mark instead of signalling
   quit.
4. Inside a recursive edit, exit it.
5. Otherwise, fall back to plain `keyboard-quit'."
  (interactive)
  ;; Step 0: if a minibuffer is active but not selected (easy to hit with
  ;; `focus-follows-mouse'/`mouse-autoselect-window', since the mouse can
  ;; wander over another window while the minibuffer still waits for
  ;; input), redirect both window and frame focus into it first.
  ;; Otherwise every check below acts on the wrong buffer, and
  ;; `minibuffer-keyboard-quit' in particular can silently no-op on some
  ;; unrelated region instead of aborting the minibuffer.
  (let ((minibuf (active-minibuffer-window)))
    (when (and minibuf (not (eq (selected-window) minibuf)))
      (select-frame-set-input-focus (window-frame minibuf))
      (select-window minibuf)))
  (let ((cmd (catch 'mortal/quit--found
               (dolist (map (current-active-maps))
                 (unless (eq map global-map)
                   (let ((binding (lookup-key map (kbd "C-g"))))
                     (when (and (commandp binding)
                                (not (eq binding 'mortal/quit)))
                       (throw 'mortal/quit--found binding)))))
               nil)))
    (cond
     (cmd (call-interactively cmd))
     ((> (minibuffer-depth) 0)
      (minibuffer-keyboard-quit))
     ((region-active-p)
      (let (select-active-regions)
        (deactivate-mark)))
     ((> (recursion-depth) 0)
      (exit-recursive-edit))
     (t (keyboard-quit)))))

;;; ---------------------------------------------------------------------------
;;; Tab line
;;; ---------------------------------------------------------------------------

(defun mortal/tab-line-select-tab (n)
  (interactive "n")
  (when-let* ((buffer (nth (1- n) (tab-line-tabs-fixed-window-buffers))))
    (switch-to-buffer buffer)))

(defun mortal/tab-line-new-tab-menu ()
  "Create and open a new empty buffer in `prog-mode`."
  (interactive)
  (let ((buffer (generate-new-buffer "new")))
    (switch-to-buffer buffer)
    (prog-mode)))

(defun mortal/tab-line-close ()
  "Close the current tab, preserving buffers displayed elsewhere."
  (interactive)
  (let* ((buffer (current-buffer))
         (elsewhere (delq (selected-window)
                          (get-buffer-window-list buffer nil t)))
         (last-tab (null (cdr (funcall tab-line-tabs-function)))))
    (if (and last-tab (window-deletable-p))
        (if elsewhere
            (delete-window)
          (kill-buffer-and-window))
      (let ((tab-line-close-tab-function
             (if elsewhere #'bury-buffer #'kill-buffer)))
        (tab-line-close-tab)))))

;;; ---------------------------------------------------------------------------
;;; Smart movement
;;; ---------------------------------------------------------------------------

(defun mortal/forward-word ()
  "Move point forward one \"smart\" step."
  (interactive)
  (deactivate-mark)
  (mortal/move 1))

(defun mortal/backward-word ()
  "Move point backward one \"smart\" step."
  (interactive)
  (deactivate-mark)
  (mortal/move -1))

(defun mortal/forward-delete-word ()
  "Delete the active region, or one smart step forward."
  (interactive "*")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (deactivate-mark)
    (let ((start (point)))
      (mortal/move 1)
      (delete-region start (point)))))

(defun mortal/backward-delete-word ()
  "Delete the active region, or one smart step backward."
  (interactive "*")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (deactivate-mark)
    (let ((end (point)))
      (mortal/move -1)
      (delete-region (point) end))))


(defun mortal/mark-move (direction)
  (unless (use-region-p)
    (push-mark (point) t t))
  (mortal/move direction)
  (setq mark-active t))

(defun mortal/mark-n-forward-word ()
  "Move point forward one smart step, extending the region."
  (interactive)
  (mortal/mark-move 1))

(defun mortal/mark-n-backward-word ()
  "Move point backward one smart step, extending the region."
  (interactive)
  (mortal/mark-move -1))

(defun mortal/mark-forward-char ()
  "Move forward one character, extending the region if active."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (forward-char 1)
  (setq mark-active t))

(defun mortal/mark-backward-char ()
  "Move backward one character, extending the region if active."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (backward-char 1)
  (setq mark-active t))

(defun mortal/deselect-forward-char ()
  "Deselect the region and move forward one character."
  (interactive)
  (deactivate-mark)
  (forward-char 1))

(defun mortal/deselect-backward-char ()
  "Deselect the region and move backward one character."
  (interactive)
  (deactivate-mark)
  (backward-char 1))


(defun mortal/deselect-previous-line ()
  "Move to the previous line and deselect the region."
  (interactive)
  (deactivate-mark)
  (condition-case nil
      (line-move -1)
    (beginning-of-buffer nil)))

(defun mortal/deselect-next-line ()
  "Move to the next line and deselect the region."
  (interactive)
  (deactivate-mark)
  (condition-case nil
      (line-move 1)
    (end-of-buffer nil)))

(defun mortal/mark-previous-line ()
  "Move to the previous line, extending the region if active."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (condition-case nil
      (line-move -1)
    (beginning-of-buffer nil))
  (setq mark-active t))

(defun mortal/mark-next-line ()
  "Move to the next line, extending the region if active."
  (interactive)
  (unless (use-region-p)
    (push-mark (point) t t))
  (condition-case nil
      (line-move 1)
    (end-of-buffer nil))
  (setq mark-active t))

;;; ---------------------------------------------------------------------------
;;; whitespace deletion
;;; ---------------------------------------------------------------------------

(defun mortal/forward-delete-whitespace ()
  "Delete region or whitespace after point without using the kill ring."
  (interactive "*")
  (let ((delete-active-region nil))
    (cond
     ((use-region-p)
      (delete-region (region-beginning) (region-end)))
     ((not (looking-at "[ \t\n]"))
      (delete-char 1 nil))
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
        (when blank-next
          (indent-according-to-mode)))))))

(defun mortal/backward-delete-whitespace ()
  "Delete region or whitespace before point without using the kill ring."
  (interactive "*")
  (let ((delete-active-region nil))
    (cond
     ((use-region-p)
      (delete-region (region-beginning) (region-end)))
     ((not (looking-back "[ \t\n]" 1))
      (delete-char -1 nil))
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
        (when blank-prev
          (indent-according-to-mode)))))))

;;; ---------------------------------------------------------------------------
;;; Indentation
;;; ---------------------------------------------------------------------------

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

(defun mortal/indent-line-or-region ()
  "Increase indentation of the current line (or region) by one
indent step. If a region is active, expand it to whole lines and
keep it selected."
  (interactive "*")
  (let* ((region-p (use-region-p))
         (beg (if region-p
                  (save-excursion
                    (goto-char (region-beginning))
                    (line-beginning-position))
                (line-beginning-position)))
         (end (if region-p
                  (save-excursion
                    (goto-char (region-end))
                    (line-end-position))
                (line-end-position)))
         (offset (mortal/current-indent-offset))
         (end-marker (copy-marker end))
         (deactivate-mark nil))
    (save-excursion
      (goto-char beg)
      (while (< (point) end-marker)
        (indent-line-to (+ (current-indentation) offset))
        (forward-line 1)))
    (when region-p
      (goto-char beg)
      (push-mark end-marker nil t))
    (set-marker end-marker nil)))

(defun mortal/complete-or-indent ()
  "If a completion-preview ghost text is showing, accept it.
Otherwise, indent."
  (interactive)
  (cond
   ;; Ghost text is currently displayed — accept it.
   ((bound-and-true-p completion-preview-active-mode)
    (completion-preview-insert))
   ;; Nothing to complete — just indent.
   (t
    (indent-for-tab-command))))



;;; ---------------------------------------------------------------------------
;;; Newline handling
;;; ---------------------------------------------------------------------------

(defun mortal/newline-and-indent-current ()
  "Insert a newline without ever reindenting the previous line.
Indent only the newly created current line."
  (interactive)
  (let (electric-indent-mode)      ; temporarily disable electric-indent's
    (newline))                     ; hooks for this one newline
  (indent-according-to-mode))      ; indent just the line we landed on

;;; ---------------------------------------------------------------------------
;;; Select all
;;; ---------------------------------------------------------------------------

;; hack for marking whole buffer without moving point, because that would move view

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

;;; ---------------------------------------------------------------------------
;;; Undo / redo
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; Commenting
;;; ---------------------------------------------------------------------------

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

;;; ---------------------------------------------------------------------------
;;; Yanking
;;; ---------------------------------------------------------------------------

(defun mortal/yank-plain ()
  "Yank plain text, replacing the active region."
  (interactive)
  (let ((text (substring-no-properties (current-kill 0))))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert text)))

;;; ---------------------------------------------------------------------------
;;; Completion
;;; ---------------------------------------------------------------------------

(defun mortal/completion-preview-or-next-candidate ()
  "Show a completion preview if none is active; otherwise cycle candidates."
  (interactive)
  (completion-preview--inhibit-update)
  (if completion-preview-active-mode
      (completion-preview-next-candidate 1)
    (unless completion-preview-mode
      (completion-preview-mode 1))
    (completion-preview--show)))

;;; ---------------------------------------------------------------------------
;;; Mark Region
;;; ---------------------------------------------------------------------------

(require 'cl-lib)

(defvar-local mortal/meaningful-mark-ring nil
  "Ring of meaningful mark snapshots.")

(add-to-list 'desktop-locals-to-save 'mortal/meaningful-mark-ring)

(defvar mortal/meaningful-mark-ring-max 12
  "Maximum number of meaningful marks to retain per buffer.")

(defvar-local mortal/meaningful-mark-ring-last-point nil)

(defun mortal/save-meaningful-mark ()
  "Save point and the current window view unless already saved."
  (let* ((window (selected-window))
         (point-pos (point))
         (start-pos (window-start window))
         (hscroll (window-hscroll window)))
    (unless (cl-some
             (lambda (entry)
               (= (plist-get entry :point) point-pos))
             mortal/meaningful-mark-ring)
      (push (list :point point-pos
                  :start start-pos
                  :hscroll hscroll)
            mortal/meaningful-mark-ring)
      (when (> (length mortal/meaningful-mark-ring)
               mortal/meaningful-mark-ring-max)
        (setq mortal/meaningful-mark-ring
              (butlast mortal/meaningful-mark-ring))))))

(defun mortal--restore-meaningful-mark (entry)
  "Restore point and window view from mark snapshot ENTRY."
  (let ((window (selected-window)))
    (goto-char (plist-get entry :point))
    (set-window-start window (plist-get entry :start) t)
    (set-window-hscroll window (plist-get entry :hscroll))))

(defun mortal/exchange-point-and-mark-or-push ()
  "Exchange point and mark, save a mark, or activate the previous mark."
  (interactive)
  (if mark-active
      (exchange-point-and-mark)
    (if (equal (point) mortal/meaningful-mark-ring-last-point)
        (when (cadr mortal/meaningful-mark-ring)
          (set-mark (plist-get (cadr mortal/meaningful-mark-ring) :point))
          (activate-mark))
      (mortal/save-meaningful-mark)
      (push-mark (point) nil)
      (setq mortal/meaningful-mark-ring-last-point (point)))))

(defun mortal/pop-meaningful-mark ()
  "Save point if new, then cycle forward through meaningful marks."
  (interactive)
  (mortal/save-meaningful-mark)
  (when mortal/meaningful-mark-ring
    (let ((entry (car mortal/meaningful-mark-ring)))
      (setq mortal/meaningful-mark-ring
            (append (cdr mortal/meaningful-mark-ring) (list entry)))
      (mortal--restore-meaningful-mark entry))))

(defun mortal/pop-meaningful-mark-reverse ()
  "Save point if new, then cycle backward through meaningful marks."
  (interactive)
  (mortal/save-meaningful-mark)
  (when mortal/meaningful-mark-ring
    (let ((entry (car (last mortal/meaningful-mark-ring))))
      (setq mortal/meaningful-mark-ring
            (cons entry (butlast mortal/meaningful-mark-ring)))
      (mortal--restore-meaningful-mark entry))))

(defun mortal/clear-meaningful-mark-ring ()
  "Clear the current buffer's meaningful mark ring."
  (interactive)
  (setq mortal/meaningful-mark-ring nil
        mortal/meaningful-mark-ring-last-point nil)
  (message "Meaningful mark ring cleared"))


;;; ---------------------------------------------------------------------------
;;; Helpers
;;; ---------------------------------------------------------------------------

(defun mortal/define-key-dynamic-fallback (keymap key fn)
  "Bind KEY to a dynamic fallback around FN.

In `prog-mode`-derived modes, always call FN.

Otherwise, find the highest-precedence active command bound to KEY
that is not FN.  For function-key events such as <tab> and <return>,
also consider their corresponding ASCII control-key forms."
  (let ((lookup-keys
         (cond
          ((equal key (kbd "<tab>"))
           (list key (kbd "TAB")))
          ((equal key (kbd "<return>"))
           (list key (kbd "RET")))
          (t
           (list key)))))
    (cl-labels
        ((wrapper ()
           (interactive)
           (if (derived-mode-p 'prog-mode)
               (call-interactively fn)
             ;; Remove our own binding so we don't find WRAPPER.
             (define-key keymap key nil t)
             (unwind-protect
                 (let (binding)
                   (dolist (lookup-key lookup-keys)
                     (unless binding
                       (setq binding
                             (cl-loop
                              for map in (current-active-maps t)
                              for candidate = (lookup-key map lookup-key)
                              when (and (commandp candidate)
                                        (not (eq candidate fn)))
                              return candidate))))
                   (call-interactively (or binding fn)))
               ;; Reinstall our wrapper.
               (define-key keymap key #'wrapper)))))
      (define-key keymap key #'wrapper))))


;;; ---------------------------------------------------------------------------
;;; Keymap definition
;;; ---------------------------------------------------------------------------


(defvar mortal-map
  (let ((map (make-sparse-keymap)))
    ;; Undefine C-* and M-* except C-m and C-i (TAB).
    (dolist (letter (number-sequence ?a ?z))
      (dolist (modifier '("C-" "M-"))
        (unless (and (equal modifier "C-")
                     (memq letter '(?i ?m)))
          (define-key
           map
           (kbd (concat modifier (char-to-string letter)))
           #'undefined))))
    map))


(defun mortal/add-keys (&rest bindings)
  "Add alternating KEY DEF pairs to `mortal-map'.

KEY is a string understood by `kbd'.

If BINDINGS contains `:fallback' followed by a non-nil value,
use dynamic fallback bindings for all subsequent KEY DEF pairs."
  (let (fallback)
    (while bindings
      (let ((item (pop bindings)))
        (if (eq item :fallback)
            (setq fallback (pop bindings))
          (let ((key (kbd item))
                (def (pop bindings)))
            (if fallback
                (mortal/define-key-dynamic-fallback
                 mortal-map key def)
              (define-key mortal-map key def))))))
    mortal-map))



;; Tab selection keys.
(dotimes (i 9)
  (let ((n (1+ i)))
    (define-key esc-map (kbd (format "M-%d" n)) nil)
    (mortal/add-keys
     (format "M-%d" n)
     (let ((tab-number n))
       (lambda ()
         (interactive)
         (mortal/tab-line-select-tab tab-number))))))


;; KEYMAP

;; Dynamic fallbacks
(mortal/add-keys
 :fallback t
 "<tab>"       #'mortal/complete-or-indent
 "<return>"    #'mortal/newline-and-indent-current
 "<up>"        #'mortal/deselect-previous-line
 "<down>"      #'mortal/deselect-next-line
 "<backspace>" #'mortal/backward-delete-whitespace
 "d"           #'self-insert-command ; somehow needed
 "<delete>"    #'mortal/forward-delete-whitespace
 ;; for vterm
 "C-l"         #'undefined
 "C-d"         #'undefined
 )

(mortal/add-keys
 ;; quitting
 "<escape>" #'mortal/quit

 ;; better deletion
 "C-<backspace>" #'mortal/backward-delete-word
 "C-<delete>" #'mortal/forward-delete-word

 ;; indent behaviour
 "<backtab>" #'mortal/unindent-line-or-region
 "S-<iso-lefttab>" #'mortal/unindent-line-or-region
 "C-<tab>" #'mortal/indent-line-or-region

 ;; completion cycling
 "M-TAB" #'mortal/completion-preview-or-next-candidate
 "M-<iso-lefttab>" #'completion-preview-prev-candidate
 
 ;; jumping
 "M-." #'xref-find-definitions
 "M-," #'xref-go-back

 ;; emacs prefixes
 "<f1>" ctl-x-map
 "<f2>" mode-specific-map
 "<f4>" help-map
 "M-x" #'execute-extended-command
 "C-g" goto-map
 "C-f" search-map

 ;; tab management
 "M-<left>" #'tab-line-switch-to-prev-tab
 "M-<right>" #'tab-line-switch-to-next-tab
 "C-w" #'mortal/tab-line-close
 "C-b" #'speedbar

 ;; emacs movement
 "M-p" #'mortal/deselect-previous-line
 "M-n" #'mortal/deselect-next-line
 "M-f" #'mortal/deselect-forward-char
 "M-b" #'mortal/deselect-backward-char

 ;; move line
 "M-<up>" #'mortal/move-line-up
 "M-<down>" #'mortal/move-line-down

 ;; deleting
 "C-k" #'mortal/delete-line
 
 ;; clipboard
 "C-c"         #'mortal/copy-line-or-region
 "C-v"         #'mortal/yank-plain
 "C-x" #'mortal/kill-line-or-region
 "C-S-c" #'mortal/copy-line-or-region

 ;; file actions
 "C-s" #'save-buffer
 "C-o" #'find-file
 "C-n" #'mortal/tab-line-new-tab-menu

 ;; undo / redo
 "C-z" #'mortal/undo
 "C-S-z" #'mortal/redo
 "C-y" #'mortal/redo

 ;; insert new line
 "C-<return>" #'mortal/insert-line-below

 ;; replace
 "C-r" #'query-replace

 ;; commenting
 "C-;" #'mortal/comment-dwim

 ;; smarter point movement
 "C-<left>" #'mortal/backward-word
 "C-<right>" #'mortal/forward-word
 "C-S-<left>" #'mortal/mark-n-backward-word
 "C-S-<right>" #'mortal/mark-n-forward-word

 ;; selection stuff
 "<left>" #'mortal/deselect-backward-char
 "<right>" #'mortal/deselect-forward-char
 "S-<left>" #'mortal/mark-backward-char
 "S-<right>" #'mortal/mark-forward-char
 "S-<up>" #'mortal/mark-previous-line
 "S-<down>" #'mortal/mark-next-line
 
 ;; useful commands
 "C-a" #'mortal/temp-select-all-dispatch
 "C-SPC" #'mortal/exchange-point-and-mark-or-push
 "C-S-SPC" #'mortal/clear-meaningful-mark-ring
 "M-SPC" #'mortal/pop-meaningful-mark
 "M-S-SPC" #'mortal/pop-meaningful-mark-reverse)
 
(provide 'mortal-keymap)
