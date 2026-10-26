;;; gptel-flymake.el --- Expose Flymake diagnostics to gptel as tools -*- lexical-binding: t; -*-

;; Version: 0.4
;; Package-Requires: ((emacs "29.1") (gptel "0.9.8"))
;; Keywords: tools, convenience, languages

;;; Commentary:

;; Registers read-only, informational gptel tools (category "flymake") that let
;; the model look at Flymake/Eglot diagnostics itself.  Nothing here edits
;; buffers or files, and no key bindings are defined.
;;
;; Tools:
;;
;;   flymake_list_buffers     Buffers with Flymake enabled and their counts.
;;   flymake_diagnostics      Diagnostics of a buffer (message, severity,
;;                            location, nearby code); filterable by severity
;;                            and line range.
;;   flymake_read_lines       Numbered lines of a buffer, to read more context.
;;   flymake_recheck          Re-run Flymake and return the fresh diagnostics
;;                            (asynchronous), e.g. to verify a fix made with
;;                            some other tool.
;;
;; Usage:
;;
;;   (require 'gptel-flymake)
;;   M-x gptel-flymake-enable        ; adds the tools to `gptel-tools'
;;
;; (or pick the "flymake" category in `gptel-menu' -> Tools).
;;
;; PRIVACY: tool results (diagnostics and source code) are sent to the model
;; backend that gptel is configured with.  The tools run without asking for
;; confirmation.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'flymake)
(require 'gptel)

(defgroup gptel-flymake nil
  "Expose Flymake diagnostics to gptel as tools."
  :group 'tools
  :prefix "gptel-flymake-")

(defcustom gptel-flymake-category "flymake"
  "gptel tool category used for the Flymake tools."
  :type 'string)

(defcustom gptel-flymake-context-lines 3
  "Default number of source lines shown before and after each diagnostic."
  :type 'integer)

(defcustom gptel-flymake-max-diagnostics 50
  "Maximum number of diagnostics returned by one tool call."
  :type 'integer)

(defcustom gptel-flymake-max-read-lines 300
  "Maximum number of lines returned by `flymake_read_lines'."
  :type 'integer)

(defcustom gptel-flymake-recheck-timeout 8.0
  "Seconds `flymake_recheck' waits for Flymake backends to finish."
  :type 'number)

;;;; Helpers

(defun gptel-flymake--safe (thunk)
  "Call THUNK and return its string result, or an error description string."
  (condition-case err
      (funcall thunk)
    (error (format "Error: %s" (error-message-string err)))))

(defun gptel-flymake--int (n)
  "Coerce tool argument N to an integer, or nil."
  (cond ((integerp n) n)
        ((numberp n) (truncate n))
        ((and (stringp n) (string-match-p "\\`[0-9]+\\'" n)) (string-to-number n))))

(defun gptel-flymake--flymake-buffers ()
  "Return live non-gptel buffers with Flymake enabled, most recent first."
  (seq-filter (lambda (b)
                (with-current-buffer b
                  (and (bound-and-true-p flymake-mode)
                       (not (bound-and-true-p gptel-mode)))))
              (buffer-list)))

(defun gptel-flymake--resolve-buffer (spec)
  "Return the buffer named or visiting SPEC (buffer name or file name).
Without SPEC, return the most recently used buffer with Flymake enabled."
  (let ((spec (and (stringp spec) (string-trim spec))))
    (if (and spec (not (string-empty-p spec)))
        (or (get-buffer spec)
            (find-buffer-visiting (expand-file-name spec))
            (cl-find-if (lambda (b)
                          (let ((f (buffer-file-name b)))
                            (and f (string= (file-name-nondirectory f)
                                            (file-name-nondirectory spec)))))
                        (buffer-list))
            (error "No buffer or visited file matches %S.  Buffers with Flymake: %s"
                   spec (or (string-join (mapcar #'buffer-name (gptel-flymake--flymake-buffers)) ", ")
                            "none")))
      (or (car (gptel-flymake--flymake-buffers))
          (error "No buffer has Flymake enabled")))))

(defun gptel-flymake--sorted-diagnostics ()
  "Return the current buffer's Flymake diagnostics sorted by position."
  (sort (flymake-diagnostics)
        (lambda (a b) (< (flymake-diagnostic-beg a) (flymake-diagnostic-beg b)))))

(defun gptel-flymake--severity (diag)
  "Return \"error\", \"warning\" or \"note\" for DIAG."
  (let ((name (symbol-name (flymake-diagnostic-type diag))))
    (cond ((string-match-p "error" name) "error")
          ((string-match-p "warn" name) "warning")
          (t "note"))))

(defun gptel-flymake--severity-rank (label)
  "Return a number ordering severity LABEL."
  (pcase label ("error" 2) ("warning" 1) (_ 0)))

(defun gptel-flymake--position (diag)
  "Return (LINE . COLUMN), 1-based, of DIAG."
  (with-current-buffer (flymake-diagnostic-buffer diag)
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (flymake-diagnostic-beg diag))
        (cons (line-number-at-pos) (1+ (current-column)))))))

(defun gptel-flymake--numbered-lines (buf first last &optional mark)
  "Return lines FIRST..LAST of BUF as \"NNN | text\"; MARK gets a `>' prefix."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- (max 1 first)))
        (let (out)
          (cl-loop for n from (max 1 first) to last
                   until (eobp)
                   do (push (format "%s%5d | %s"
                                    (if (eql n mark) ">" " ") n
                                    (buffer-substring-no-properties
                                     (line-beginning-position) (line-end-position)))
                            out)
                   (forward-line 1))
          (string-join (nreverse out) "\n"))))))

;;;; Reporting

(cl-defun gptel-flymake--report (buf &key min-severity start end context)
  "Describe the diagnostics of BUF.
MIN-SEVERITY, START and END (line numbers) filter; CONTEXT is the number
of source lines shown around each diagnostic."
  (with-current-buffer buf
    (unless (bound-and-true-p flymake-mode)
      (error "Flymake is not enabled in buffer %s" (buffer-name buf)))
    (let* ((ctx (or context gptel-flymake-context-lines))
           (min-rank (gptel-flymake--severity-rank (or min-severity "note")))
           (all (gptel-flymake--sorted-diagnostics))
           (count (lambda (sev) (cl-count sev all :key #'gptel-flymake--severity :test #'string=)))
           (running (flymake-running-backends))
           (disabled (flymake-disabled-backends))
           rows)
      (dolist (d all)
        (let* ((pos (gptel-flymake--position d))
               (line (car pos)))
          (when (and (>= (gptel-flymake--severity-rank (gptel-flymake--severity d)) min-rank)
                     (or (null start) (>= line start))
                     (or (null end) (<= line end)))
            (push (cons d pos) rows))))
      (setq rows (nreverse rows))
      (let* ((shown (seq-take rows gptel-flymake-max-diagnostics))
            (header
             (concat
              (format "Buffer: %s (mode: %s)%s\n"
                      (buffer-name) major-mode
                      (if (buffer-modified-p) ", has unsaved changes" ""))
              (format "Flymake: %d diagnostics (%d errors, %d warnings, %d notes)"
                      (length all) (funcall count "error")
                      (funcall count "warning") (funcall count "note"))
              (if (/= (length rows) (length all))
                  (format "; %d match the filters" (length rows)) "")
              (if running
                  (format "\nFlymake is still running (%s); results may be incomplete.  \
Use flymake_recheck." (mapconcat #'symbol-name running ", "))
                "")
              (if disabled
                  (format "\nDisabled backends: %s" (mapconcat #'symbol-name disabled ", "))
                "")
              (if (> (length rows) (length shown))
                  (format "\nShowing the first %d only; narrow with start_line/end_line."
                          (length shown))
                ""))))
        (if (null shown)
            header
          (concat
           header "\n\n"
           (string-join
            (cl-loop
             for (d . pos) in shown
             for i from 1
             collect
             (concat
              (format "[%d] %s at line %d, column %d (source: %s)\n    %s"
                      i (gptel-flymake--severity d) (car pos) (cdr pos)
                      (flymake-diagnostic-backend d)
                      (replace-regexp-in-string
                       "\n" "\n    " (flymake-diagnostic-text d)))
              (if (> ctx 0)
                  (concat "\n    code ('>' marks the diagnostic line):\n"
                          (gptel-flymake--numbered-lines
                           buf (- (car pos) ctx) (+ (car pos) ctx) (car pos)))
                "")))
            "\n\n")))))))

;;;; Tool implementations

(defun gptel-flymake--tool-list-buffers ()
  "List buffers with Flymake enabled."
  (let ((bufs (gptel-flymake--flymake-buffers)))
    (if (null bufs)
        "No buffer has Flymake enabled."
      (string-join
       (mapcar
        (lambda (b)
          (with-current-buffer b
            (let ((all (flymake-diagnostics)))
              (format "%s | %s | %d errors, %d warnings, %d notes"
                      (buffer-name) major-mode
                      (cl-count "error" all :key #'gptel-flymake--severity :test #'string=)
                      (cl-count "warning" all :key #'gptel-flymake--severity :test #'string=)
                      (cl-count "note" all :key #'gptel-flymake--severity :test #'string=)))))
        bufs)
       "\n"))))

(defun gptel-flymake--tool-diagnostics (buffer min-severity start-line end-line context-lines)
  "Implementation of `flymake_diagnostics'."
  (gptel-flymake--report (gptel-flymake--resolve-buffer buffer)
                         :min-severity min-severity
                         :start (gptel-flymake--int start-line)
                         :end (gptel-flymake--int end-line)
                         :context (gptel-flymake--int context-lines)))

(defun gptel-flymake--tool-read-lines (buffer start-line end-line)
  "Implementation of `flymake_read_lines'."
  (let* ((buf (gptel-flymake--resolve-buffer buffer))
         (total (with-current-buffer buf
                  (save-restriction (widen) (count-lines (point-min) (point-max)))))
         (start (max 1 (or (gptel-flymake--int start-line) 1)))
         (wanted (or (gptel-flymake--int end-line)
                     (+ start gptel-flymake-max-read-lines -1)))
         (end (min wanted total (+ start gptel-flymake-max-read-lines -1))))
    (if (> start total)
        (format "Buffer %s has only %d lines." (buffer-name buf) total)
      (format "%s, lines %d-%d of %d%s\n%s"
              (buffer-name buf) start end total
              (if (< end (min wanted total))
                  (format " (capped at %d lines per call)" gptel-flymake-max-read-lines)
                "")
              (gptel-flymake--numbered-lines buf start end)))))

(defun gptel-flymake--tool-recheck (callback buffer)
  "Implementation of `flymake_recheck'; deliver the result to CALLBACK."
  (let ((buf (gptel-flymake--resolve-buffer buffer)))
    (with-current-buffer buf
      (unless (bound-and-true-p flymake-mode)
        (error "Flymake is not enabled in buffer %s" (buffer-name buf)))
      (flymake-start nil t))
    (let ((started (float-time))
          timer)
      (setq timer
            (run-with-timer
             0.5 0.5
             (lambda ()
               (let* ((elapsed (- (float-time) started))
                      (live (buffer-live-p buf))
                      (timed-out (>= elapsed gptel-flymake-recheck-timeout)))
                 (when (or (not live) timed-out
                           (and (>= elapsed 1.0)
                                (null (with-current-buffer buf (flymake-running-backends)))))
                   (cancel-timer timer)
                   (funcall
                    callback
                    (gptel-flymake--safe
                     (lambda ()
                       (if (not live)
                           "The buffer was killed while rechecking."
                         (concat (if timed-out
                                     "Note: Flymake did not finish within the timeout.\n" "")
                                 (gptel-flymake--report buf :context 0)))))))))))))
  nil)

;;;; Tool registration

(defvar gptel-flymake-tools nil
  "The gptel tool structs created by `gptel-flymake-register'.")

(defun gptel-flymake--wrap (fn)
  "Return a synchronous tool function that calls FN and never signals."
  (lambda (&rest args)
    (gptel-flymake--safe (lambda () (apply fn args)))))

(defun gptel-flymake--wrap-async (fn)
  "Return an asynchronous tool function that calls FN and never signals."
  (lambda (callback &rest args)
    (condition-case err
        (apply fn callback args)
      (error (funcall callback (format "Error: %s" (error-message-string err)))))))

;;;###autoload
(defun gptel-flymake-register ()
  "Create and register the Flymake tools with gptel.  Return the tool list."
  (let ((cat gptel-flymake-category)
        (buffer-arg '(:name "buffer"
                      :type string
                      :optional t
                      :description "Buffer name or file name.  Defaults to the most \
recently used buffer that has Flymake enabled.")))
    (setq gptel-flymake-tools
          (list
           (gptel-make-tool
            :name "flymake_list_buffers"
            :function (gptel-flymake--wrap #'gptel-flymake--tool-list-buffers)
            :description "List Emacs buffers that have Flymake enabled, with their \
major mode and the number of errors, warnings and notes."
            :args nil
            :category cat)
           (gptel-make-tool
            :name "flymake_diagnostics"
            :function (gptel-flymake--wrap #'gptel-flymake--tool-diagnostics)
            :description "Get the Flymake diagnostics (compiler, linter or LSP errors \
and warnings) of an Emacs buffer: message, severity, line and column, reporting \
backend, and the nearby source code.  Read-only.  Use this first when asked about \
errors or warnings in the user's code."
            :args (list buffer-arg
                        '(:name "min_severity"
                          :type string
                          :enum ["error" "warning" "note"]
                          :optional t
                          :description "Only return diagnostics at least this severe.")
                        '(:name "start_line"
                          :type integer
                          :optional t
                          :description "Only return diagnostics on or after this line.")
                        '(:name "end_line"
                          :type integer
                          :optional t
                          :description "Only return diagnostics on or before this line.")
                        '(:name "context_lines"
                          :type integer
                          :optional t
                          :description "Source lines to show before and after each \
diagnostic (0 for none)."))
            :category cat)
           (gptel-make-tool
            :name "flymake_read_lines"
            :function (gptel-flymake--wrap #'gptel-flymake--tool-read-lines)
            :description "Read numbered lines of an Emacs buffer, e.g. to see more code \
around a diagnostic or the whole file.  Read-only.  Line-number prefixes are not \
part of the text."
            :args (list buffer-arg
                        '(:name "start_line"
                          :type integer
                          :optional t
                          :description "First line to read (default 1).")
                        '(:name "end_line"
                          :type integer
                          :optional t
                          :description "Last line to read (default: a capped block)."))
            :category cat)
           (gptel-make-tool
            :name "flymake_recheck"
            :function (gptel-flymake--wrap-async #'gptel-flymake--tool-recheck)
            :async t
            :description "Re-run Flymake on a buffer, wait for it to finish, and return \
the fresh diagnostics.  Use it to verify whether a fix has resolved the \
diagnostics.  Does not modify the buffer."
            :args (list buffer-arg)
            :category cat)))))

;;;###autoload
(defun gptel-flymake-enable ()
  "Register the Flymake tools and add them to `gptel-tools'."
  (interactive)
  (gptel-flymake-disable)
  (gptel-flymake-register)
  (dolist (tool gptel-flymake-tools)
    (cl-pushnew tool gptel-tools))
  (message "gptel-flymake: %d tools enabled (category %S)"
           (length gptel-flymake-tools) gptel-flymake-category))

;;;###autoload
(defun gptel-flymake-disable ()
  "Remove the Flymake tools from `gptel-tools'."
  (interactive)
  (setq gptel-tools (cl-remove-if (lambda (tool) (memq tool gptel-flymake-tools))
                                  gptel-tools)))

;; Register on load so the tools show up in `gptel-menu'; they are only sent
;; to the model once enabled there or via `gptel-flymake-enable'.
(gptel-flymake-register)

(provide 'gptel-flymake)
;;; gptel-flymake.el ends here
