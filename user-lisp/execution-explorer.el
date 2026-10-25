;;; execution-explorer.el --- Buffer inspection and overlays over JSON-RPC -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable `execution-explorer-mode', a global minor mode, to start a
;; localhost JSON-RPC server.  The buffer that is current at that moment
;; is the default buffer for file.current and buffer.contents; if it is
;; killed, the buffer shown in the selected window is used instead.
;; Disabling the mode stops the server and removes all overlays that the
;; explorer created, in every buffer.
;;
;; Methods:
;;   file.current              - Get the activation buffer's file name and path.
;;   buffer.list               - List Emacs buffers.
;;   buffer.contents           - Get a buffer's contents (default: activation buffer).
;;   overlay.set               - Replace the overlays of one node in a specified buffer.
;;   overlay.delete            - Delete the overlays of one node by its node_id.
;;   overlay.clear             - Clear all node overlays in a specified buffer.
;;   exception.overlays.set    - Append persistent exception overlays.
;;   delete.exception_overlays - Delete all exception overlays in all buffers.
;;
;; Overlay parameters:
;;   node_id      - Required for overlay.set and overlay.delete.
;;   buffer       - Existing Emacs buffer name or file path.
;;   overlays     - Array of overlay specifications.
;;   line_start   - First line, 1-based and inclusive.
;;   line_end     - Last line, 1-based and inclusive.
;;   column_start - First visual column.
;;   column_end   - Last visual column.
;;   hover        - Optional tooltip text (the "value").
;;   target       - Optional, exception overlays only.  Makes the overlay
;;                  clickable; see "Following exceptions" below.
;;
;; Each specification creates one overlay per line in its line range.
;; Line and column coordinates are only used to *place* an overlay.  From
;; then on Emacs repositions it with its default overlay semantics:
;;
;;   - Text inserted or deleted before an overlay moves it along.
;;   - Text inserted at the start of an overlay, or strictly inside it,
;;     becomes part of it.  Text inserted at the end is outside it.
;;   - Deleting part of an overlay shrinks it.  Deleting all of its text
;;     removes it (`evaporate'); overlays created empty are never removed.
;;
;; Nesting is inferred from the *current* extent of the overlays.
;; Insertions cannot change which overlay contains which, so only
;; deletions trigger a coalesced, idle-time recomputation of priorities.
;; Exception overlays always keep the highest priority.
;;
;; Overlay styles (`execution-explorer-overlay-style'):
;;
;;   box     - The default.  The region keeps its text; a box is drawn
;;             around it on hover (value) or permanently (exception), and
;;             the value is shown as a tooltip.
;;   display - The source text stays visible and is annotated inline:
;;               (source_code_span===value_of_hover_text)
;;               (source_code_span=!!value_of_exception_hover_text)
;;             No boxes and no spaces.  The buffer text itself is
;;             untouched; the parens, operator and value are plain
;;             overlay strings.  A multi-line region gets
;;             "(" on its first line and the operator, value and ")"
;;             after its last line.  Overlays without a value keep the
;;             box style.
;;
;; Switch interactively with `execution-explorer-toggle-overlay-style'
;; or `execution-explorer-set-overlay-style'.
;;
;; Following exceptions:
;;
;; An exception overlay can lead to a deeper exception, and that one to
;; a still deeper one, like the frames of a stack trace.  Give the
;; exception specification a `target':
;;
;;   {"line_start": 10, "line_end": 10, "column_start": 4, "column_end": 20,
;;    "hover": "ValueError: bad input",
;;    "target": [{"file": "/src/a.py", "line": 42, "column": 4,
;;                "hover": "raised in parse()"},
;;               {"file": "/src/b.py", "line": 7,
;;                "hover": "raised in check()"}]}
;;
;; The array is ordered from the next frame to the deepest.  Equivalent
;; nested form: each target object may carry its own `target' (but an
;; object inside an array must not).  A target has:
;;
;;   file   - Buffer name, visited file name, or a path on disk.
;;   line   - 1-based line.
;;   column - Optional visual column (default 0).
;;   hover  - Optional exception text shown at the destination.
;;
;; Press RET or click (mouse-1 / mouse-2) on the overlay to jump to the
;; first target.  There the explorer marks the line with an exception
;; overlay of its own, which is clickable in turn if the chain continues.
;; An exception overlay that is already at the destination is left as it
;; is.  C-RET jumps straight to the deepest target.  Jumps push the xref
;; marker stack, so `xref-go-back' (M-,) returns you.
;;
;;; Code:

(require 'jsonrpc)
(require 'subr-x)
(require 'cl-lib)
(require 'pcase)
(require 'tooltip)
(require 'xref)

;;;; Customization and state

(defgroup execution-explorer nil
  "Inspect Emacs buffers and set overlays over JSON-RPC."
  :group 'tools)

(defcustom execution-explorer-port 9257
  "TCP port for the localhost JSON-RPC server."
  :type 'integer
  :group 'execution-explorer)

(defcustom execution-explorer-value-overlay-color "white"
  "Color of the hover box drawn around value overlays.
In the `display' style it is the box color around the shown value."
  :type 'color
  :group 'execution-explorer)

(defcustom execution-explorer-exception-overlay-color "red"
  "Color of the persistent box drawn around exception overlays.
In the `display' style it is the box color around the exception value."
  :type 'color
  :group 'execution-explorer)

(defcustom execution-explorer-overlay-style 'box
  "How overlays are presented.
`box' keeps the text and boxes it (tooltip shows the value).
`display' wraps the region inline as \"(span===value)\" or, for
exceptions, \"(span=!!exception-value)\", with no boxes."
  :type '(choice (const :tag "Box with tooltip" box)
                 (const :tag "Inline (span===value)" display))
  :group 'execution-explorer)

(defconst execution-explorer--exception-priority most-positive-fixnum
  "Priority of exception overlays.
Value overlays get 1 + their nesting depth, so they stay below this.")

(defconst execution-explorer--refresh-delay 0.05
  "Idle seconds to wait before recomputing priorities after a deletion.")

(defvar execution-explorer--buffer nil
  "The buffer that was current when `execution-explorer-mode' was enabled.
It is the default buffer for file.current and buffer.contents while it
is live, or nil when the mode is off.")

(defvar execution-explorer--server nil
  "The listening server process.")

(defvar execution-explorer--clients nil
  "Connected JSON-RPC client connections.")

(defvar-local execution-explorer--nodes nil
  "Hash table mapping node ids to lists of managed overlays in this buffer.")

(defvar-local execution-explorer--exception-overlays nil
  "List of persistent exception overlays in this buffer.")

(defvar-local execution-explorer--mouse-motions-active nil
  "Non-nil if mouse-motion tracking was enabled in this buffer by us.")

(defvar-local execution-explorer--refresh-dirty nil
  "Non-nil if a deletion may have changed how overlays nest.")

(defvar-local execution-explorer--refresh-timer nil
  "Pending idle timer that will recompute priorities, or nil.")

;;;; Small utilities

(defmacro execution-explorer--with-rollback (cleanup &rest body)
  "Run BODY; if it exits non-locally, run CLEANUP.  Return BODY's value."
  (declare (indent 1))
  (let ((ok (make-symbol "ok")))
    `(let (,ok)
       (unwind-protect
           (prog1 (progn ,@body)
             (setq ,ok t))
         (unless ,ok ,cleanup)))))

(defun execution-explorer--param (object key)
  "Get KEY from JSON OBJECT.
KEY is a keyword, such as :buffer or :line_start.
Support plists, alists, and hash tables."
  (let* ((string-key (substring (symbol-name key) 1))
         (symbol-key (intern string-key))
         (missing (make-symbol "missing")))
    (cond
     ((hash-table-p object)
      (let ((value (cl-loop for k in (list string-key key symbol-key)
                            for v = (gethash k object missing)
                            unless (eq v missing) return v
                            finally return missing)))
        (unless (eq value missing) value)))
     ((keywordp (car-safe object))
      (plist-get object key))
     ((listp object)
      (cdr (or (assoc string-key object)
               (assq symbol-key object)
               (assq key object)))))))

(defun execution-explorer--node-id (params)
  "Return the validated :node_id from PARAMS."
  (let ((node-id (execution-explorer--param params :node_id)))
    (unless (or (integerp node-id)
                (and (stringp node-id) (not (string-empty-p node-id))))
      (error "The node_id parameter must be a non-empty string or integer: %S"
             node-id))
    node-id))

(defun execution-explorer--overlay-specs (params)
  "Return the :overlays parameter of PARAMS as a list."
  (let ((specs (execution-explorer--param params :overlays)))
    (unless (or (listp specs) (vectorp specs))
      (error "The overlays parameter must be an array: %S" specs))
    (append specs nil)))

;;;; Buffer selection

(defun execution-explorer--activation-buffer ()
  "Return the activation buffer.
This is the buffer in which the mode was enabled while it is live,
otherwise the buffer shown in the selected window."
  (if (buffer-live-p execution-explorer--buffer)
      execution-explorer--buffer
    (window-buffer (or (minibuffer-selected-window) (selected-window)))))

(defun execution-explorer--file-matches-p (buffer target)
  "Return non-nil if BUFFER visits a file matching TARGET.
TARGET may be a full path or just a file name."
  (let ((file (buffer-local-value 'buffer-file-name buffer)))
    (and file
         (or (equal file (expand-file-name target))
             (equal (file-name-nondirectory file) target)))))

(defun execution-explorer--resolve-buffer (params)
  "Resolve the buffer specified by the :buffer parameter in PARAMS."
  (let ((target (execution-explorer--param params :buffer)))
    (unless (and (stringp target) (not (string-empty-p target)))
      (error "Invalid :buffer parameter: %S" target))
    (or (get-buffer target)
        (cl-find-if (lambda (buffer)
                      (execution-explorer--file-matches-p buffer target))
                    (buffer-list))
        (error "Cannot find an open buffer named %S or a matching file"
               target))))

;;;; Overlay predicates

(defun execution-explorer--managed-overlay-p (overlay)
  "Return non-nil if OVERLAY is managed by execution-explorer."
  (eq (overlay-get overlay 'category) 'execution-explorer))

(defun execution-explorer--exception-overlay-p (overlay)
  "Return non-nil if OVERLAY is an exception overlay."
  (equal (overlay-get overlay 'execution-explorer-type) "exception"))

(defun execution-explorer--live-overlay-p (overlay)
  "Return non-nil if OVERLAY is still alive in the current buffer.
Overlays whose text was deleted are removed by Emacs (see `evaporate')
and report no buffer."
  (and (overlayp overlay)
       (eq (overlay-buffer overlay) (current-buffer))))

(defun execution-explorer--delete-overlays (overlays)
  "Delete the live OVERLAYS in the current buffer; return how many."
  (let ((count 0))
    (dolist (ov overlays)
      (when (execution-explorer--live-overlay-p ov)
        (delete-overlay ov)
        (cl-incf count)))
    count))

;;;; Mouse tracking and hover text

(defun execution-explorer--mouse-motion (event)
  "Hide stale tooltips and schedule a fresh tooltip for EVENT.
Interactive only so it can be bound to `mouse-movement'; it needs a
mouse event and is hidden from `M-x' completion."
  (declare (completion ignore))
  (interactive "e")
  (execution-explorer--flush-refresh)
  (tooltip-hide)
  (when (and (eventp event)
             (car (mouse-pixel-position)))
    (setq tooltip-last-mouse-motion-event (copy-sequence event))
    (tooltip-start-delayed-tip)))

(defun execution-explorer--enable-mouse-motions ()
  "Enable mouse-motion events in the current buffer."
  (unless execution-explorer--mouse-motions-active
    (setq-local execution-explorer--mouse-motions-active t)
    (setq-local track-mouse t)
    (local-set-key [mouse-movement] #'execution-explorer--mouse-motion)))

(defun execution-explorer--disable-mouse-motions ()
  "Restore the previous mouse-motion settings in the current buffer."
  (when execution-explorer--mouse-motions-active
    (kill-local-variable 'execution-explorer--mouse-motions-active)
    (kill-local-variable 'track-mouse)
    (local-unset-key [mouse-movement])))

(defun execution-explorer--hover-text-at (position)
  "Return the hover text of the innermost managed overlay at POSITION.
Exception overlays win over value overlays; among value overlays the
most deeply nested one wins."
  (when (integer-or-marker-p position)
    ;; Make sure priorities reflect any deletion since the last refresh.
    (execution-explorer--flush-refresh)
    (cl-some
     (lambda (ov)
       (when (execution-explorer--managed-overlay-p ov)
         (let ((text (overlay-get ov 'execution-explorer-hover)))
           (and (stringp text) (not (string-empty-p text)) text))))
     (overlays-at position t))))

(defun execution-explorer--hover-echo (window object position)
  "Return the hover text for an execution-explorer overlay at POSITION.
This is a `help-echo' function; OBJECT is the overlay (or buffer)
and WINDOW the window the pointer is in."
  (let ((buffer (cond ((and (overlayp object) (overlay-buffer object)))
                      ((bufferp object) object)
                      ((window-live-p window) (window-buffer window))
                      (t (current-buffer)))))
    (with-current-buffer buffer
      (execution-explorer--hover-text-at position))))

;;;; Geometry
;;
;; Overlay extents are the source of truth: Emacs keeps the start and end
;; of every overlay up to date through edits.  Nothing about an overlay's
;; position is cached; nesting is derived from the live extents.

(defun execution-explorer--overlay-range (ov)
  "Return OV's current extent as (START . END) buffer positions."
  (cons (overlay-start ov) (overlay-end ov)))

(defun execution-explorer--range-contains-p (outer inner)
  "Return non-nil if range OUTER strictly contains range INNER.
Both are (START . END) conses; equal ranges do not contain each other."
  (and (<= (car outer) (car inner))
       (>= (cdr outer) (cdr inner))
       (not (equal outer inner))))

(defun execution-explorer--range-depths (ranges)
  "Return an `equal' hash table mapping each of RANGES to its depth.
Depth is the length of the longest chain of ranges containing it.
Ranges are sorted so that any container precedes what it contains,
which lets each depth be computed in one pass."
  (let ((depths (make-hash-table :test #'equal))
        (sorted (sort (delete-dups (copy-sequence ranges))
                      (lambda (a b)
                        (or (< (car a) (car b))
                            (and (= (car a) (car b))
                                 (> (cdr a) (cdr b)))))))
        (done nil))
    (dolist (range sorted)
      (let ((depth 0))
        (dolist (candidate done)
          (when (execution-explorer--range-contains-p candidate range)
            (setq depth (max depth (1+ (gethash candidate depths))))))
        (puthash range depth depths)
        (push range done)))
    depths))

(defun execution-explorer--goto-line (line)
  "Move to the start of 1-based LINE in the accessible portion of the buffer.
Return non-nil if LINE exists."
  (goto-char (point-min))
  (or (= line 1)
      (and (= (forward-line (1- line)) 0)
           ;; Moving onto the end of a last line without a trailing
           ;; newline does not reach a new line.
           (eq (char-before) ?\n))))

(defun execution-explorer--next-line ()
  "Move to the start of the next line; return non-nil if there is one."
  (and (= (forward-line 1) 0)
       (eq (char-before) ?\n)))

;;;; Overlay creation

(defconst execution-explorer--value-operator "==="
  "Operator shown between a source span and its value.")

(defconst execution-explorer--exception-operator "=!!"
  "Operator shown between a source span and its exception value.")

(defun execution-explorer--display-p (ov)
  "Return non-nil if OV should be annotated inline with its value.
This needs the `display' style and a non-empty value."
  (let ((hover (overlay-get ov 'execution-explorer-hover)))
    (and (eq execution-explorer-overlay-style 'display)
         (stringp hover)
         (not (string-empty-p hover)))))

(defun execution-explorer--style-overlay (ov)
  "Apply the style-dependent appearance (and exception priority) to OV.
In the `box' style, value overlays show a box on hover and exception
overlays show a persistent box.  In the `display' style the source text
stays visible and is wrapped, without boxes or spaces, as
\"(SPAN===VALUE)\", or \"(SPAN=!!EXCEPTION-VALUE)\" for exceptions.
A multi-line region gets the opening paren on its first line and the 
operator, value and closing paren after its last line.  
Exception overlays always have the highest priority."
  (let* ((exception (execution-explorer--exception-overlay-p ov))
         (color (if exception
                    execution-explorer-exception-overlay-color
                  execution-explorer-value-overlay-color))
         (box (list :box (list :line-width 1 :color color))))
    (if (execution-explorer--display-p ov)
        (let ((operator (if exception
                            execution-explorer--exception-operator
                          execution-explorer--value-operator)))
          (overlay-put ov 'face nil)
          (overlay-put ov 'mouse-face nil)
          (overlay-put ov 'before-string
                       (and (overlay-get ov 'execution-explorer-primary)
                            "("))
          (overlay-put ov 'after-string
                       (and (overlay-get ov 'execution-explorer-last)
                            (format "%s%s)" operator
                                    (overlay-get ov 'execution-explorer-hover)))))
      (overlay-put ov 'before-string nil)
      (overlay-put ov 'after-string nil)
      (overlay-put ov 'face (and exception box))
      (overlay-put ov 'mouse-face (unless exception box)))
    (when exception
      (overlay-put ov 'priority execution-explorer--exception-priority))))

(defun execution-explorer--make-overlay (start end hover type node-id)
  "Create a managed overlay from START to END in the current buffer.
Emacs' default insertion types apply.  A non-empty overlay evaporates
when all of its text is deleted."
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'category 'execution-explorer)
    (overlay-put ov 'execution-explorer-hover hover)
    (overlay-put ov 'execution-explorer-type type)
    (overlay-put ov 'help-echo #'execution-explorer--hover-echo)
    (when node-id
      (overlay-put ov 'execution-explorer-node node-id))
    ;; Setting `evaporate' on an empty overlay would delete it at once.
    (when (< start end)
      (overlay-put ov 'evaporate t))
    (execution-explorer--style-overlay ov)
    ov))

(defun execution-explorer--create-line-overlays (rect hover type node-id lenient)
  "Create one overlay per line of RECT in the current buffer.
RECT is (LINE-START LINE-END COLUMN-START COLUMN-END) and is only used
to place the overlays.  Return the list of new overlays.  The first
one is marked primary: in the `display' style it alone shows the value.

If LENIENT is non-nil, lines that do not exist or are too short to
cover the column range are skipped.  Otherwise a nonexistent line
signals an error.  If anything signals, the overlays already created
here are deleted, so a call either creates everything or nothing."
  (let (created)
    (execution-explorer--with-rollback (mapc #'delete-overlay created)
      (cl-destructuring-bind (line-start line-end column-start column-end) rect
        (save-excursion
          (save-restriction
            (widen)
            (let ((reachable (execution-explorer--goto-line line-start)))
              (cl-loop
               for line from line-start to line-end
               do
               (if (not reachable)
                   (unless lenient
                     (error "Line %d is outside the buffer" line))
                 (let* ((bol (point))
                        (start (progn (move-to-column column-start) (point)))
                        (end (progn (move-to-column column-end) (point))))
                   (when (or (not lenient) (< start end))
                     (push (execution-explorer--make-overlay
                            start end hover type node-id)
                           created))
                   (goto-char bol)))
               (when (< line line-end)
                 (setq reachable (and reachable
                                      (execution-explorer--next-line))))))))))
    (setq created (nreverse created))
    (when created
      (overlay-put (car created) 'execution-explorer-primary t)
      (overlay-put (car (last created)) 'execution-explorer-last t)
      (execution-explorer--style-overlay (car created))
      (execution-explorer--style-overlay (car (last created))))
    created))

(defun execution-explorer--parse-spec (spec &optional type-override)
  "Validate overlay SPEC and return (RECT HOVER TYPE).
RECT is (LINE-START LINE-END COLUMN-START COLUMN-END).  If
TYPE-OVERRIDE is non-nil it replaces any type given in SPEC."
  (let ((rect (mapcar (lambda (key) (execution-explorer--param spec key))
                      '(:line_start :line_end :column_start :column_end)))
        (hover (execution-explorer--param spec :hover))
        (type (or type-override
                  (execution-explorer--param spec :type)
                  "value")))
    (cl-destructuring-bind (line-start line-end column-start column-end) rect
      (unless (and (integerp line-start)
                   (integerp line-end)
                   (integerp column-start)
                   (integerp column-end)
                   (>= line-start 1)
                   (>= line-end line-start)
                   (>= column-start 0)
                   (>= column-end column-start))
        (error "Invalid overlay coordinates: %S" spec)))
    (unless (or (null hover) (stringp hover))
      (error "Overlay hover must be a string or null: %S" hover))
    (unless (member type '("value" "exception"))
      (error "Invalid overlay type: %S" type))
    (list rect hover type)))

;;;; Following exceptions
;;
;; A target is the list (FILE LINE COLUMN HOVER NEXT), where NEXT is the
;; target after this one, or nil at the end of the chain.  An exception
;; overlay stores the first target of its chain.  Following it jumps
;; there and marks the destination with a new exception overlay that
;; stores NEXT, so the chain continues one click at a time.

(defvar execution-explorer-exception-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'execution-explorer-follow-exception)
    (define-key map [mouse-2] #'execution-explorer-follow-exception)
    (define-key map (kbd "RET") #'execution-explorer-follow-exception)
    (define-key map [C-return] #'execution-explorer-follow-exception-deepest)
    map)
  "Keymap active on exception overlays that have a target.")

(defun execution-explorer--parse-target-1 (spec next)
  "Validate one target SPEC and return (FILE LINE COLUMN HOVER NEXT)."
  (let ((file (execution-explorer--param spec :file))
        (line (execution-explorer--param spec :line))
        (column (or (execution-explorer--param spec :column) 0))
        (hover (execution-explorer--param spec :hover)))
    (unless (and (stringp file) (not (string-empty-p file))
                 (integerp line) (>= line 1)
                 (integerp column) (>= column 0)
                 (or (null hover) (stringp hover)))
      (error "Invalid exception target: %S" spec))
    (list file line column hover next)))

(defun execution-explorer--parse-target (target)
  "Validate TARGET and return its chain as nested lists, or nil.
TARGET is an object, optionally carrying its own :target, or an array
of objects ordered from the next frame to the deepest."
  (cond
   ((null target) nil)
   ((vectorp target)
    (let (next)
      (dolist (item (nreverse (append target nil)) next)
        (when (execution-explorer--param item :target)
          (error "Targets inside an array must not nest: %S" item))
        (setq next (execution-explorer--parse-target-1 item next)))))
   (t
    (execution-explorer--parse-target-1
     target
     (execution-explorer--parse-target
      (execution-explorer--param target :target))))))

(defun execution-explorer--make-clickable (overlays target)
  "Make OVERLAYS follow TARGET when clicked.  Do nothing if TARGET is nil."
  (when target
    (dolist (ov overlays)
      (overlay-put ov 'execution-explorer-target target)
      (overlay-put ov 'keymap execution-explorer-exception-map)
      (overlay-put ov 'pointer 'hand))))

(defun execution-explorer--target-buffer (file)
  "Return a buffer for FILE (buffer name, visited file, or path on disk)."
  (or (get-buffer file)
      (cl-find-if (lambda (b) (execution-explorer--file-matches-p b file))
                  (buffer-list))
      (and (file-exists-p file) (find-file-noselect file))
      (user-error "Cannot find buffer or file %S" file)))

(defun execution-explorer--mark-destination (line column hover next)
  "Mark LINE from COLUMN to its end with an exception overlay.
Use the current buffer.  The overlay shows HOVER and is clickable if
NEXT is non-nil.  If an exception overlay is already at point, leave it
as it is.  Point must be at the destination."
  (unless (cl-some #'execution-explorer--exception-overlay-p
                   (overlays-at (point)))
    (let ((created (execution-explorer--create-line-overlays
                    (list line line column most-positive-fixnum)
                    hover "exception" nil t)))
      (when created
        (execution-explorer--make-clickable created next)
        (setq execution-explorer--exception-overlays
              (append created execution-explorer--exception-overlays))
        (execution-explorer--refresh-overlays)))))

(defun execution-explorer--visit-target (target)
  "Jump to TARGET and mark it.  Return the next target of the chain, or nil."
  (pcase-let* ((`(,file ,line ,column ,hover ,next) target)
               (dest (execution-explorer--target-buffer file)))
    (pop-to-buffer dest)
    (with-current-buffer dest
      (save-restriction
        (widen)
        (unless (execution-explorer--goto-line line)
          (user-error "Line %d is outside %s" line (buffer-name dest))))
      (move-to-column column)
      (execution-explorer--mark-destination line column hover next))
    next))

(defun execution-explorer--target-at (event)
  "Return the exception target at the mouse EVENT, or at point."
  (let* ((posn (and (mouse-event-p event) (event-start event)))
         (window (and posn (posn-window posn)))
         (pos (or (and posn (posn-point posn)) (point)))
         (buffer (if (window-live-p window)
                     (window-buffer window)
                   (current-buffer))))
    (with-current-buffer buffer
      (cl-some (lambda (ov) (overlay-get ov 'execution-explorer-target))
               (overlays-at pos t)))))

;;;###autoload
(defun execution-explorer-follow-exception (event &optional deepest)
  "Jump to the exception target at the click EVENT or at point.
The destination is marked with an exception overlay that continues the
chain, if there is more of it.  With prefix argument DEEPEST, follow
the whole chain and stop at its last target."
  (interactive (list last-input-event current-prefix-arg))
  (let ((target (execution-explorer--target-at event)))
    (unless target
      (user-error "No exception target here"))
    (xref-push-marker-stack)            ; so M-, brings you back
    (let ((next (execution-explorer--visit-target target)))
      (when deepest
        (while next
          (setq next (execution-explorer--visit-target next)))))))

;;;###autoload
(defun execution-explorer-follow-exception-deepest (event)
  "Follow the whole exception chain at EVENT or point, to its deepest target."
  (interactive (list last-input-event))
  (execution-explorer-follow-exception event t))

;;;; Node table and overlay bookkeeping

(defun execution-explorer--node-table ()
  "Return the current buffer's node table, creating it if needed."
  (or execution-explorer--nodes
      (setq execution-explorer--nodes
            (make-hash-table :test #'equal))))

(defun execution-explorer--managed-overlays ()
  "Return all live managed overlays in the current buffer.
This covers node overlays and exception overlays.  Overlays that
Emacs has deleted (their text was removed) are pruned from the
bookkeeping as a side effect."
  (let (result changed)
    (when execution-explorer--nodes
      (maphash
       (lambda (node-id overlays)
         (let ((live (cl-remove-if-not #'execution-explorer--live-overlay-p
                                       overlays)))
           (setq result (append live result))
           (unless (= (length live) (length overlays))
             (push (cons node-id live) changed))))
       execution-explorer--nodes)
      ;; Mutate the table only after `maphash' has finished.
      (dolist (entry changed)
        (if (cdr entry)
            (puthash (car entry) (cdr entry) execution-explorer--nodes)
          (remhash (car entry) execution-explorer--nodes))))
    (setq execution-explorer--exception-overlays
          (cl-remove-if-not #'execution-explorer--live-overlay-p
                            execution-explorer--exception-overlays))
    (append execution-explorer--exception-overlays result)))

(defun execution-explorer--add-node-overlays (node-id spec)
  "Create line-by-line overlays from SPEC for NODE-ID.
Return the number of line overlays created.  Callers must call
`execution-explorer--refresh-overlays' afterwards."
  (let* ((parsed (execution-explorer--parse-spec spec))
         (table (execution-explorer--node-table))
         (created (execution-explorer--create-line-overlays
                   (nth 0 parsed) (nth 1 parsed) (nth 2 parsed)
                   node-id nil)))
    (puthash node-id (append created (gethash node-id table)) table)
    (length created)))

(defun execution-explorer--make-exception-overlays (spec)
  "Create persistent exception overlays from SPEC.
Return the list of new overlays; the caller registers them.  Lines that
do not exist or are too short for the column range are skipped.  If SPEC
has a :target, the overlays are clickable and jump along its chain.  The
target is validated before anything is created."
  (let* ((parsed (execution-explorer--parse-spec spec "exception"))
         (target (execution-explorer--parse-target
                  (execution-explorer--param spec :target)))
         (created (execution-explorer--create-line-overlays
                   (nth 0 parsed) (nth 1 parsed) "exception" nil t)))
    (execution-explorer--make-clickable created target)
    created))

(defun execution-explorer--delete-node (node-id)
  "Delete NODE-ID's overlays in the current buffer.
Return the number of overlays deleted.  Callers must call
`execution-explorer--refresh-overlays' afterwards."
  (let* ((table (execution-explorer--node-table))
         (count (execution-explorer--delete-overlays (gethash node-id table))))
    (remhash node-id table)
    count))

(defun execution-explorer--clear-node-overlays ()
  "Delete all node overlays in the current buffer, then refresh.
Exception overlays are kept; they have their own lifecycle."
  (when execution-explorer--nodes
    (maphash (lambda (_node-id overlays)
               (execution-explorer--delete-overlays overlays))
             execution-explorer--nodes)
    (clrhash execution-explorer--nodes))
  (execution-explorer--refresh-overlays))

(defun execution-explorer--clear-everything ()
  "Delete all node and exception overlays in every live buffer.
Also removes the change hooks, timers and mouse tracking installed by
the explorer, by way of `execution-explorer--refresh-overlays'."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or execution-explorer--nodes
                execution-explorer--exception-overlays)
        (execution-explorer--delete-overlays
         execution-explorer--exception-overlays)
        (setq execution-explorer--exception-overlays nil)
        (execution-explorer--clear-node-overlays)))))

;;;; Change tracking and refresh
;;
;; Emacs moves overlay boundaries for us, so nothing has to run on a
;; plain insertion: with the default insertion types, overlays that
;; share a start or an end keep sharing it, so containment is preserved.
;; A deletion can however collapse boundaries together or turn a partial
;; overlap into containment, so deletions mark the priorities dirty and
;; schedule one coalesced recomputation.

(defun execution-explorer--after-change (_beg _end len)
  "Note that a deletion or replacement of LEN characters happened."
  (when (> len 0)
    (setq execution-explorer--refresh-dirty t)
    (execution-explorer--schedule-refresh)))

(defun execution-explorer--schedule-refresh ()
  "Schedule a single idle-time refresh for the current buffer."
  (unless execution-explorer--refresh-timer
    (setq execution-explorer--refresh-timer
          (run-with-idle-timer
           execution-explorer--refresh-delay nil
           #'execution-explorer--run-scheduled-refresh (current-buffer)))))

(defun execution-explorer--cancel-refresh-timer ()
  "Cancel the pending refresh timer of the current buffer, if any."
  (when (timerp execution-explorer--refresh-timer)
    (cancel-timer execution-explorer--refresh-timer))
  (setq execution-explorer--refresh-timer nil))

(defun execution-explorer--run-scheduled-refresh (buffer)
  "Timer function: refresh BUFFER if it is still alive."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq execution-explorer--refresh-timer nil)
      (execution-explorer--flush-refresh))))

(defun execution-explorer--flush-refresh ()
  "Refresh priorities now if a deletion made them stale."
  (when execution-explorer--refresh-dirty
    (execution-explorer--refresh-overlays)))

(defun execution-explorer--refresh-overlays ()
  "Recompute priorities and appearance of managed overlays.
Nesting depth of value overlays is derived from their current extents.
Exception overlays always get the highest priority.  Also install or
remove the change hook and mouse tracking depending on what is left."
  (setq execution-explorer--refresh-dirty nil)
  (execution-explorer--cancel-refresh-timer)
  (let* ((overlays (execution-explorer--managed-overlays))
         (value-overlays
          (cl-remove-if #'execution-explorer--exception-overlay-p overlays)))
    ;; Keep the change hook installed only while overlays exist.
    (if overlays
        (add-hook 'after-change-functions
                  #'execution-explorer--after-change nil t)
      (remove-hook 'after-change-functions
                   #'execution-explorer--after-change t))
    ;; Mouse tracking (for tooltips) is needed only for value overlays
    ;; in the box style; the display style shows values in the buffer.
    (if (and value-overlays (eq execution-explorer-overlay-style 'box))
        (execution-explorer--enable-mouse-motions)
      (execution-explorer--disable-mouse-motions))
    (let ((depths (execution-explorer--range-depths
                   (mapcar #'execution-explorer--overlay-range
                           value-overlays))))
      (dolist (ov overlays)
        (execution-explorer--style-overlay ov)
        (unless (execution-explorer--exception-overlay-p ov)
          (overlay-put
           ov 'priority
           (1+ (gethash (execution-explorer--overlay-range ov) depths 0))))))))

;;;; Overlay style commands

(defun execution-explorer--apply-style-everywhere ()
  "Re-style the managed overlays of every live buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (or execution-explorer--nodes
                execution-explorer--exception-overlays)
        (execution-explorer--refresh-overlays)))))

;;;###autoload
(defun execution-explorer-set-overlay-style (style)
  "Set the overlay STYLE (`box' or `display') and re-style all overlays."
  (interactive
   (list (intern (completing-read
                  "Overlay style: " '("box" "display") nil t nil nil
                  (symbol-name execution-explorer-overlay-style)))))
  (unless (memq style '(box display))
    (user-error "Unknown overlay style: %S" style))
  (setq execution-explorer-overlay-style style)
  (execution-explorer--apply-style-everywhere)
  (message "execution-explorer overlay style: %s" style))

;;;###autoload
(defun execution-explorer-toggle-overlay-style ()
  "Switch between the `box' and `display' overlay styles."
  (interactive)
  (execution-explorer-set-overlay-style
   (if (eq execution-explorer-overlay-style 'box) 'display 'box)))

;;;; JSON-RPC method handlers

(defun execution-explorer--rpc-file-current (_params)
  "Handle file.current."
  (with-current-buffer (execution-explorer--activation-buffer)
    (list :name (and buffer-file-name (file-name-nondirectory buffer-file-name))
          :path buffer-file-name)))

(defun execution-explorer--rpc-buffer-list (_params)
  "Handle buffer.list."
  (vconcat
   (mapcar (lambda (buffer)
             (list :name (buffer-name buffer)
                   :path (buffer-local-value 'buffer-file-name buffer)))
           (buffer-list))))

(defun execution-explorer--rpc-buffer-contents (params)
  "Handle buffer.contents."
  (with-current-buffer (if (execution-explorer--param params :buffer)
                           (execution-explorer--resolve-buffer params)
                         (execution-explorer--activation-buffer))
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun execution-explorer--rpc-overlay-set (params)
  "Handle overlay.set: replace the overlays of one node.
The call is atomic: on error the node ends up with no overlays."
  (let ((node-id (execution-explorer--node-id params))
        (buffer (execution-explorer--resolve-buffer params))
        (specs (execution-explorer--overlay-specs params))
        (count 0))
    (with-current-buffer buffer
      (unwind-protect
          (execution-explorer--with-rollback
              (execution-explorer--delete-node node-id)
            (execution-explorer--delete-node node-id)
            (dolist (spec specs)
              (cl-incf count
                       (execution-explorer--add-node-overlays node-id spec))))
        (execution-explorer--refresh-overlays)))
    (message "execution-explorer: created %d line overlays for node %S"
             count node-id)
    (list :node_id node-id :count count)))

(defun execution-explorer--rpc-overlay-delete (params)
  "Handle overlay.delete.
If :buffer is supplied, search only that buffer; otherwise search all
live buffers."
  (let ((node-id (execution-explorer--node-id params))
        (buffers (if (execution-explorer--param params :buffer)
                     (list (execution-explorer--resolve-buffer params))
                   (buffer-list)))
        (deleted 0))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when execution-explorer--nodes
          (let ((count (execution-explorer--delete-node node-id)))
            (when (> count 0)
              (cl-incf deleted count)
              (execution-explorer--refresh-overlays))))))
    (list :node_id node-id :deleted deleted)))

(defun execution-explorer--rpc-overlay-clear (params)
  "Handle overlay.clear.
Clears the node overlays of the specified buffer.  Exception overlays
are left alone; remove them with delete.exception_overlays."
  (with-current-buffer (execution-explorer--resolve-buffer params)
    (execution-explorer--clear-node-overlays)
    t))

(defun execution-explorer--rpc-exception-overlays-set (params)
  "Handle exception.overlays.set: append exception overlays to a buffer.
Previously created exception overlays are kept.  The call is atomic: if
any specification is invalid, none of its overlays are created."
  (let ((buffer (execution-explorer--resolve-buffer params))
        (specs (execution-explorer--overlay-specs params))
        (created nil))
    (with-current-buffer buffer
      (unwind-protect
          (execution-explorer--with-rollback (mapc #'delete-overlay created)
            (dolist (spec specs)
              (setq created
                    (append created
                            (execution-explorer--make-exception-overlays spec))))
            (setq execution-explorer--exception-overlays
                  (append created execution-explorer--exception-overlays)))
        (execution-explorer--refresh-overlays)))
    (list :count (length created))))

(defun execution-explorer--rpc-delete-exception-overlays (_params)
  "Handle delete.exception_overlays: delete them in all buffers."
  (let ((deleted 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when execution-explorer--exception-overlays
          (cl-incf deleted (execution-explorer--delete-overlays
                            execution-explorer--exception-overlays))
          (setq execution-explorer--exception-overlays nil)
          (execution-explorer--refresh-overlays))))
    (list :deleted deleted)))

;;;; JSON-RPC dispatch

(defconst execution-explorer--methods
  '(("file.current"              . execution-explorer--rpc-file-current)
    ("buffer.list"               . execution-explorer--rpc-buffer-list)
    ("buffer.contents"           . execution-explorer--rpc-buffer-contents)
    ("overlay.set"               . execution-explorer--rpc-overlay-set)
    ("overlay.delete"            . execution-explorer--rpc-overlay-delete)
    ("overlay.clear"             . execution-explorer--rpc-overlay-clear)
    ("exception.overlays.set"    . execution-explorer--rpc-exception-overlays-set)
    ("delete.exception_overlays" . execution-explorer--rpc-delete-exception-overlays))
  "Alist mapping JSON-RPC method names to handler functions.")

(defun execution-explorer--dispatch-request (method params)
  "Handle JSON-RPC METHOD with PARAMS."
  (let* ((name (if (symbolp method) (symbol-name method) method))
         (handler (cdr (assoc name execution-explorer--methods))))
    (unless handler
      (error "Unknown JSON-RPC method: %s" name))
    (funcall handler params)))

(defun execution-explorer--dispatch (_connection method params)
  "Dispatch a JSON-RPC request and report useful error messages."
  (condition-case err
      (execution-explorer--dispatch-request method params)
    (error
     (let ((msg (error-message-string err)))
       (message "execution-explorer RPC failed: method=%S error=%s params=%S"
                method msg params)
       (jsonrpc-error "%s" msg)))))

;;;; Server lifecycle

(defun execution-explorer--accept (_server client _message)
  "Wrap accepted CLIENT socket in a JSON-RPC connection."
  (push (make-instance
         'jsonrpc-process-connection
         :name (process-name client)
         :process client
         :request-dispatcher #'execution-explorer--dispatch
         :notification-dispatcher #'ignore
         :on-shutdown
         (lambda (conn)
           (setq execution-explorer--clients
                 (delq conn execution-explorer--clients))))
        execution-explorer--clients))

(defun execution-explorer--start ()
  "Start the listener if it is not already running."
  (unless (process-live-p execution-explorer--server)
    (when (processp execution-explorer--server)
      (delete-process execution-explorer--server))
    (setq execution-explorer--server
          (make-network-process
           :name "execution-explorer"
           :server t
           :host 'local
           :family 'ipv4
           :service execution-explorer-port
           :coding 'utf-8-emacs-unix
           :noquery t
           :log #'execution-explorer--accept))
    (message "execution-explorer listening on 127.0.0.1:%d"
             execution-explorer-port)))

(defun execution-explorer--stop ()
  "Close all client connections and stop the listener."
  (dolist (client (copy-sequence execution-explorer--clients))
    (condition-case err
        (jsonrpc-shutdown client)
      (error
       (message "execution-explorer client shutdown failed: %S" err))))
  (when (processp execution-explorer--server)
    (delete-process execution-explorer--server))
  (setq execution-explorer--server nil
        execution-explorer--clients nil))

;;;###autoload
(define-minor-mode execution-explorer-mode
  "Serve buffer information and overlays over localhost JSON-RPC.
This is a global mode.  The buffer that is current when it is enabled
becomes the default buffer for file.current and buffer.contents.
Disabling it stops the server and deletes all overlays created by it."
  :global t
  :lighter " Explorer"
  (if execution-explorer-mode
      (progn
        (setq execution-explorer--buffer (current-buffer))
        (execution-explorer--start))
    (setq execution-explorer--buffer nil)
    (execution-explorer--stop)
    (execution-explorer--clear-everything)))

(provide 'execution-explorer)

;;; execution-explorer.el ends here
