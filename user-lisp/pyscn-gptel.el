;;; pyscn-gptel.el --- gptel tools for pyscn (Python code quality analysis) -*- lexical-binding: t; -*-

;; Mirrors the six tools of the pyscn MCP server (pyscn-mcp) as native
;; gptel tools by calling the `pyscn' CLI directly, so no MCP client
;; (mcp.el) is needed.
;;
;;   analyze_code      -> pyscn analyze --json
;;   check_complexity  -> pyscn analyze --json --select complexity
;;   detect_clones     -> pyscn analyze --json --select clones
;;   check_coupling    -> pyscn analyze --json --select cbo
;;   find_dead_code    -> pyscn analyze --json --select deadcode
;;   get_health_score  -> pyscn analyze --json (summary only, if present)
;;
;; Requirements: `pyscn' on PATH (pipx install pyscn / uv tool install pyscn
;; / brew install pyscn), and gptel.
;;
;; Usage:
;;   (load "/path/to/pyscn-gptel.el")
;;   M-x gptel-menu -> "Tools" -> select the "pyscn" category
;; or in config:
;;   (setq gptel-tools (append gptel-tools (pyscn-gptel-tools)))
;;
;; NOTE: the --select names for clones/coupling are my best reading of the
;; CLI; confirm with `pyscn analyze --help' and adjust
;; `pyscn-gptel-select-names' if they differ.

;;; Code:

(require 'gptel)
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup pyscn-gptel nil
  "gptel tools backed by pyscn."
  :group 'gptel)

(defcustom pyscn-gptel-executable "pyscn"
  "Path or name of the pyscn executable.
Set to e.g. \"uvx\" and adjust `pyscn-gptel-extra-args-prefix' if you
do not want a global install."
  :type 'string)

(defcustom pyscn-gptel-extra-args-prefix nil
  "Arguments inserted before the pyscn subcommand.
For uvx use: (\"pyscn@latest\")."
  :type '(repeat string))

(defcustom pyscn-gptel-config nil
  "Optional path to a .pyscn.toml, passed via PYSCN_CONFIG."
  :type '(choice (const nil) file))

(defcustom pyscn-gptel-max-output 60000
  "Truncate tool output to this many characters."
  :type 'integer)

(defcustom pyscn-gptel-select-names
  '((complexity . "complexity")
    (clones     . "clones")
    (coupling   . "cbo")
    (dead-code  . "deadcode")
    (deps       . "deps"))
  "Mapping from analysis symbols to pyscn --select names."
  :type '(alist :key-type symbol :value-type string))

;;;; Core runner

(defun pyscn-gptel--run (path &optional select)
  "Run pyscn analyze on PATH, optionally restricted to SELECT.
Return the output string (JSON on success, error text otherwise)."
  (let* ((target (expand-file-name path))
         (_ (unless (file-exists-p target)
              (error "Path does not exist: %s" target)))
         (process-environment
          (if pyscn-gptel-config
              (cons (concat "PYSCN_CONFIG=" (expand-file-name pyscn-gptel-config))
                    process-environment)
            process-environment))
         (args (append pyscn-gptel-extra-args-prefix
                       (list "analyze" "--json" "--output" "-")
                       (when select (list "--select" select))
                       (list target))))
    (unless (executable-find pyscn-gptel-executable)
      (error "Cannot find `%s' on PATH; install with `pipx install pyscn'"
             pyscn-gptel-executable))
    (with-temp-buffer
      (let* ((default-directory (if (file-directory-p target)
                                    (file-name-as-directory target)
                                  (file-name-directory target)))
             (code (apply #'call-process pyscn-gptel-executable nil
                          (list (current-buffer) nil) nil args))
             (out (buffer-string)))
        (cond
         ((not (zerop code))
          (format "pyscn exited with code %s:\n%s"
                  code (pyscn-gptel--truncate out)))
         (t (pyscn-gptel--truncate out)))))))

(defun pyscn-gptel--truncate (s)
  "Truncate S to `pyscn-gptel-max-output' characters."
  (if (> (length s) pyscn-gptel-max-output)
      (concat (substring s 0 pyscn-gptel-max-output)
              (format "\n\n[... truncated, %d chars total]" (length s)))
    s))

(defun pyscn-gptel--select (key)
  (alist-get key pyscn-gptel-select-names))

;;;; Tool functions

(defun pyscn-gptel-analyze-code (path &optional analyses)
  "Comprehensive analysis of PATH.  ANALYSES is a vector/list of names."
  (let* ((names (when analyses
                  (mapcar (lambda (a)
                            (pcase a
                              ("complexity" (pyscn-gptel--select 'complexity))
                              ((or "clone" "clones") (pyscn-gptel--select 'clones))
                              ("cbo" (pyscn-gptel--select 'coupling))
                              ("dead_code" (pyscn-gptel--select 'dead-code))
                              ("deps" (pyscn-gptel--select 'deps))
                              (_ a)))
                          (append analyses nil))))
         (select (when names (string-join names ","))))
    (pyscn-gptel--run path select)))

(defun pyscn-gptel-check-complexity (path)
  (pyscn-gptel--run path (pyscn-gptel--select 'complexity)))

(defun pyscn-gptel-detect-clones (path)
  (pyscn-gptel--run path (pyscn-gptel--select 'clones)))

(defun pyscn-gptel-check-coupling (path)
  (pyscn-gptel--run path (pyscn-gptel--select 'coupling)))

(defun pyscn-gptel-find-dead-code (path)
  (pyscn-gptel--run path (pyscn-gptel--select 'dead-code)))

(defun pyscn-gptel-get-health-score (path)
  "Return the overall health score for PATH.
Tries to extract just the summary from the JSON report; falls back to
the (truncated) full report if the structure is not recognized."
  (let ((raw (pyscn-gptel--run path nil)))
    (condition-case nil
        (let* ((data (json-parse-string raw :object-type 'alist))
               (summary (or (alist-get 'summary data)
                            (alist-get 'health data)
                            (alist-get 'health_score data))))
          (if summary
              (json-encode summary)
            raw))
      (error raw))))

;;;; Tool definitions

(defconst pyscn-gptel--path-arg
  '(:name "path" :type string
    :description "Path to a Python file or directory to analyze."))

(defun pyscn-gptel-tools ()
  "Return the list of gptel tool structs for pyscn."
  (list
   (gptel-make-tool
    :name "pyscn_analyze_code"
    :category "pyscn"
    :function #'pyscn-gptel-analyze-code
    :description "Comprehensive Python code quality analysis with pyscn: complexity, dead code, clones, class coupling and dependency/architecture issues. Returns a JSON report."
    :args (list pyscn-gptel--path-arg
                '(:name "analyses" :type array :optional t
                  :items (:type string
                          :enum ["complexity" "dead_code" "clone" "cbo" "deps"])
                  :description "Subset of analyses to run. Omit to run all."))
    :confirm nil
    :include t)
   (gptel-make-tool
    :name "pyscn_check_complexity"
    :category "pyscn"
    :function #'pyscn-gptel-check-complexity
    :description "Cyclomatic/cognitive complexity of functions in a Python file or directory. Use to find functions that are hard to read and test."
    :args (list pyscn-gptel--path-arg)
    :include t)
   (gptel-make-tool
    :name "pyscn_detect_clones"
    :category "pyscn"
    :function #'pyscn-gptel-detect-clones
    :description "Detect duplicated or structurally similar Python code (Type 1-4 clones). Use before refactoring to find merge candidates."
    :args (list pyscn-gptel--path-arg)
    :include t)
   (gptel-make-tool
    :name "pyscn_check_coupling"
    :category "pyscn"
    :function #'pyscn-gptel-check-coupling
    :description "Class coupling (CBO) metrics for Python classes. Use to find classes that depend on too many others."
    :args (list pyscn-gptel--path-arg)
    :include t)
   (gptel-make-tool
    :name "pyscn_find_dead_code"
    :category "pyscn"
    :function #'pyscn-gptel-find-dead-code
    :description "Find unreachable (dead) code in Python files using control-flow analysis."
    :args (list pyscn-gptel--path-arg)
    :include t)
   (gptel-make-tool
    :name "pyscn_get_health_score"
    :category "pyscn"
    :function #'pyscn-gptel-get-health-score
    :description "Overall code health score (0-100) with an A-F grade for a Python file or directory."
    :args (list pyscn-gptel--path-arg)
    :include t)))

;;;###autoload
(defun pyscn-gptel-register ()
  "Register the pyscn tools with gptel (adds to `gptel-tools')."
  (interactive)
  (let ((tools (pyscn-gptel-tools)))
    (setq gptel-tools
          (append (cl-remove-if
                   (lambda (tl) (equal (gptel-tool-category tl) "pyscn"))
                   gptel-tools)
                  tools))
    (message "Registered %d pyscn tools with gptel" (length tools))))

(provide 'pyscn-gptel)
;;; pyscn-gptel.el ends here
