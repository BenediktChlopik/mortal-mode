;;; got.el --- Graph of Thoughts via mcp.el, gptel and graph-fa2 -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: b.chlopik@pm.me
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Use the Adaptive Graph of Thoughts MCP server
;; (https://github.com/SaptaDey/Adaptive-Graph-of-Thoughts-MCP-server)
;; from Emacs and draw the thought graph it computes.
;;
;;   mcp.el     starts the Python server over stdio and speaks MCP.
;;   gptel      owns the LLM and model, and calls the server's tools.
;;   graph-fa2  draws the thought graph as SVG in a buffer.
;;
;; These three are soft dependencies, loaded lazily the first time they
;; are needed, so `(require 'got)' is cheap and starts nothing.
;;
;; The server runs the reasoning itself and returns a `graph_state'.
;; `got-ask' has gptel call the server's tool and pass that graph_state
;; back as a JSON block, which this package parses, saves and draws.  The
;; server needs Neo4j; see its README.
;;
;; Every graph is saved to a project-local file, `.got.json' in the project
;; root (see `got-store-file-name').  Each `got-ask' adds a new run and
;; never changes earlier ones.  `got-load' brings the whole graph back in
;; a later session, `got-expand' asks the server to extend a stored
;; thought, and `got-add-thought' adds one by hand.  The file is plain
;; JSON, so it can be committed, diffed and edited.
;;
;; Emacs starts the server as a child process by running `got-server-script'
;; over stdio.  That script must not print anything to stdout except the
;; MCP protocol.
;;
;; Setup:
;;
;;   (add-to-list 'load-path "/path/to/got-emacs")
;;   (setq got-server-script
;;         "/path/to/Adaptive-Graph-of-Thoughts-MCP-server/launch.sh")
;;   (require 'got)
;;   (keymap-global-set "C-c g" got-command-map)   ; optional
;;
;; The server starts on the first operation that needs it, and the
;; command then continues by itself once the server is up.
;;
;; Commands:
;;
;;   `got-ask'                  ask the server; saves and draws the result
;;   `got-load'                 load this project's saved graph
;;   `got-expand'               extend a saved thought via the server
;;   `got-add-thought'          add a thought by hand
;;   `got-store-open'           visit the project's .got.json
;;   `got-graph-from-buffer'    save and draw the last ```json block
;;   `got-graph-import-region'  add JSON from the region to the graph
;;   `got-graph-import-file'    add JSON from a file to the graph
;;   `got-graph-show'           redraw the last graph
;;   `got-graph-demo'           draw a built-in sample (needs no server)
;;   `got-graph-freeze'         stop the graph moving now
;;   `got-graph-doctor'         diagnose a graph that does not appear
;;   `got-tools'                list the tools the server exposes
;;   `got-start', `got-stop'    start or stop the server explicitly

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'pcase)
(require 'project)
(require 'subr-x)

;; Loaded lazily; declared so the byte-compiler stays quiet.
(defvar mcp-hub-servers)
(defvar mcp-server-connections)
(defvar gptel-use-tools)
(defvar gptel-tools)
(defvar gptel-post-response-functions)
(defvar graph-fa2-engine)
(defvar graph-fa2-simulation-frames)
(defvar graph-fa2-framerate)
(defvar graph-fa2-ctx)
(defvar graph-fa2--player-timer)
(defvar graph-fa2--current-frame)
(defvar graph-fa2-node-clicked-functions)
(declare-function gptel "gptel")
(declare-function gptel-send "gptel")
(declare-function gptel-get-tool "gptel")
(declare-function gptel-mcp-connect "gptel-integrations")
(declare-function mcp-stop-server "mcp")
(declare-function mcp--tools "mcp")
(declare-function graph-fa2-start "graph-fa2")
(declare-function graph-fa2-ctx-bg-timer "graph-fa2")

(defgroup got nil
  "Graph of Thoughts via MCP, gptel and graph-fa2."
  :group 'tools
  :prefix "got-")

;;;; Configuration

(defcustom got-server-script
  "/home/raptus/projects/Adaptive-Graph-of-Thoughts-MCP-server/launch.sh"
  "Script that starts the MCP server over stdio.
Emacs runs it as a child process and talks MCP over its stdin and stdout."
  :type 'file)

(defcustom got-server-args nil
  "Extra command-line arguments passed to `got-server-script'."
  :type '(repeat string))

(defcustom got-server-env '(("MCP_TRANSPORT_TYPE" . "stdio"))
  "Environment variables for the server, as an alist of (NAME . VALUE)."
  :type '(alist :key-type string :value-type string))

(defcustom got-server-name "adaptive-graph-of-thoughts"
  "Name under which the server is registered in `mcp-hub-servers'."
  :type 'string)

(defcustom got-start-timeout 30
  "Seconds to wait for a lazily started server before giving up."
  :type 'number)

(defcustom got-tools-list
  '("scientific_reasoning_query"
    "analyze_research_hypothesis"
    "explore_scientific_relationships"
    "validate_scientific_claims")
  "Server tools `got-ask' can call.  The first is the default."
  :type '(repeat string))

(defcustom got-ask-prompt
  (concat "Call the `%1$s' tool with the text below as its query or main "
          "input, and ask it to include the graph state and reasoning trace "
          "if it has such options. Then output the graph state from the "
          "tool's result, exactly as returned and unabridged, as ONE "
          "```json code block. Do not invent, summarise or reformat it. "
          "If the result contains no graph state, say so and do not "
          "make one up. After the code block, give a short summary of "
          "the answer and its confidence.\n\n"
          "Input: %2$s")
  "Prompt sent by `got-ask'.
It is a `format' string: %1$s is the tool name and %2$s the user's text."
  :type 'string)

(defcustom got-graph-node-keys
  '((id      . (id node_id thought_id name))
    (content . (content label text thought statement hypothesis claim
                        description result output))
    (score   . (score confidence value rating))
    (type    . (type node_type operation op kind))
    (status  . (status state))
    (parents . (parents parent_ids parents_ids inputs sources source parent)))
  "Candidate JSON keys for each node field, tried in order."
  :type '(alist :key-type symbol :value-type (repeat symbol)))

(defcustom got-graph-max-nodes 500
  "Refuse to import a single result with more thoughts than this.
Protects Emacs from a runaway model output."
  :type 'natnum)

(defcustom got-store-enabled t
  "When non-nil, save every graph to the project's store file.
When nil, graphs are drawn but never written to disk."
  :type 'boolean)

(defcustom got-store-file-name ".got.json"
  "Name of the store file, relative to the project root.
The store holds every thought and run of a project as readable JSON, so
it can be committed, diffed, loaded with `got-load' and extended."
  :type 'string)

(defcustom got-store-file nil
  "Store file to use instead of `got-store-file-name' in the project root.
Set it per project in `.dir-locals.el' to keep the graph elsewhere."
  :type '(choice (const :tag "Project root default" nil) file))

(defcustom got-graph-colours
  '(("problem"      . "#cdd6f4")
    ("sub_question" . "#74c7ec")
    ("hypothesis"   . "#89b4fa")
    ("evidence"     . "#a6e3a1")
    ("synthesis"    . "#cba6f7")
    ("answer"       . "#f9e2af")
    ("generate"     . "#89b4fa")
    ("score"        . "#f9e2af")
    ("select"       . "#a6e3a1")
    ("aggregate"    . "#cba6f7")
    ("refine"       . "#fab387"))
  "Node colour by operation type."
  :type '(alist :key-type string :value-type string))

(defcustom got-graph-default-colour "#9399b2"
  "Colour for types missing from `got-graph-colours'."
  :type 'string)

(defcustom got-graph-buffer-name "*got-graph*"
  "Name of the graph buffer."
  :type 'string)

(defcustom got-graph-static t
  "When non-nil, let the layout settle, then freeze it.
The graph is laid out in 2D and playback is stopped once the simulation
has played to its last frame, so nodes stop moving.  Zoom, pan and
clicking keep working."
  :type 'boolean)

(defcustom got-graph-freeze-timeout 180
  "Seconds to wait for playback to reach its last frame before giving up.
If it never does, the graph is left running, never blank."
  :type 'number)

(defvar-keymap got-command-map
  :doc "Prefix map for Graph of Thoughts commands.  Bind it, e.g. to C-c g."
  "a" #'got-ask
  "b" #'got-graph-from-buffer
  "d" #'got-graph-demo
  "e" #'got-expand
  "f" #'got-graph-freeze
  "g" #'got-graph-show
  "k" #'got-stop
  "l" #'got-load
  "n" #'got-add-thought
  "o" #'got-store-open
  "s" #'got-start
  "t" #'got-tools
  "?" #'got-graph-doctor)

;;;; Server lifecycle (lazy)

(defvar got--starting nil
  "Non-nil while a start has been requested but not yet finished.")

(defvar got--waiters nil
  "Operations waiting for the server, oldest first: (READY-P FN DEADLINE).")

(defvar got--wait-timer nil "Timer polling `got--waiters'.")

(defun got--load-backend ()
  "Load mcp.el and gptel, or signal a helpful error."
  (dolist (feature '(mcp-hub gptel gptel-integrations))
    (unless (require feature nil t)
      (user-error "got: cannot load `%s'; install mcp.el and gptel" feature)))
  (unless (fboundp 'gptel-mcp-connect)
    (user-error "got: your gptel has no `gptel-mcp-connect'; please upgrade gptel")))

(defun got--env-plist ()
  "Return `got-server-env' as the keyword plist mcp.el expects."
  (cl-loop for (name . value) in got-server-env
           append (list (intern (concat ":" name)) value)))

(defun got--register ()
  "Add the GoT server to `mcp-hub-servers', replacing any old entry."
  (let ((script (expand-file-name got-server-script)))
    (unless (file-exists-p script)
      (user-error "Set `got-server-script': %s does not exist" script))
    (unless (file-executable-p script)
      (user-error "`got-server-script' is not executable (chmod +x %s)" script))
    (setq mcp-hub-servers
          (cons `(,got-server-name
                  :command ,script
                  ,@(and got-server-args `(:args ,got-server-args))
                  ,@(and got-server-env `(:env ,(got--env-plist))))
                (cl-remove got-server-name mcp-hub-servers
                           :key #'car :test #'equal)))))

(defun got--connected-p ()
  "Return non-nil when mcp.el has a connection to the server."
  (and (boundp 'mcp-server-connections)
       (hash-table-p mcp-server-connections)
       (gethash got-server-name mcp-server-connections)))

(defun got--tools ()
  "Return gptel tool structs registered for the GoT server, or nil."
  (and (featurep 'gptel)
       (ignore-errors (gptel-get-tool (list (concat "mcp-" got-server-name))))))

;;;###autoload
(defun got-start ()
  "Start the GoT server and make its tools available to gptel."
  (interactive)
  (got--load-backend)
  (got--register)
  (setq got--starting t)
  (message "got: starting %s ..." got-server-name)
  (gptel-mcp-connect (list got-server-name) nil))

;;;###autoload
(defun got-stop ()
  "Stop the GoT server and drop any operations waiting for it."
  (interactive)
  (got--load-backend)
  (mcp-stop-server got-server-name)
  (setq got--starting nil
        got--waiters nil)
  (got--stop-wait-timer)
  (message "got: stopped"))

(defun got--stop-wait-timer ()
  "Cancel the timer that polls for server readiness."
  (when (timerp got--wait-timer)
    (cancel-timer got--wait-timer))
  (setq got--wait-timer nil))

(defun got--tick ()
  "Run waiting operations whose server is ready; expire the late ones."
  (let ((waiters got--waiters)
        (now (float-time))
        pending)
    (setq got--waiters nil)
    (dolist (w waiters)
      (pcase-let ((`(,ready-p ,fn ,deadline) w))
        (cond
         ((funcall ready-p)
          (setq got--starting nil)
          (condition-case err
              (funcall fn)
            (error (message "got: %s" (error-message-string err)))))
         ((> now deadline)
          (setq got--starting nil)
          (message "got: server not ready after %ss; see *Messages* and M-x mcp-hub"
                   got-start-timeout))
         (t (push w pending)))))
    ;; Operations queued by callbacks while we ran stay after the old ones.
    (setq got--waiters (append (nreverse pending) got--waiters))
    (unless got--waiters
      (got--stop-wait-timer))))

(defun got--when-ready (ready-p fn)
  "Call FN once READY-P gives non-nil, starting the server if needed.
When the server is not up yet this returns at once and FN runs later.
Several waiting operations share one polling timer."
  (if (funcall ready-p)
      (funcall fn)
    (unless (or got--starting (got--connected-p))
      (got-start))
    (message "got: waiting for the server, will continue when it is ready...")
    (setq got--waiters
          (append got--waiters
                  (list (list ready-p fn (+ (float-time) got-start-timeout)))))
    (unless got--wait-timer
      (setq got--wait-timer (run-at-time 0.5 0.5 #'got--tick)))
    nil))

;;;; Tool listing

(defun got--show-tools ()
  "Display the tools of the connected server in a buffer."
  (let ((tools (mcp--tools (got--connected-p))))
    (with-current-buffer (get-buffer-create "*got-tools*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (tool tools)
          (insert (format "%s\n  %s\n\n"
                          (plist-get tool :name)
                          (or (plist-get tool :description) ""))))
        (goto-char (point-min)))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;;###autoload
(defun got-tools ()
  "Show the tools the server exposes, starting it first if necessary."
  (interactive)
  (got--when-ready #'got--connected-p #'got--show-tools))

;;;; Parsing: JSON result -> graph

(defun got--alistp (x)
  "Return non-nil if X looks like a JSON object parsed into an alist."
  (and (consp x) (consp (car x)) (symbolp (caar x))))

(defun got--get (alist keys)
  "Return the first non-nil value in ALIST among KEYS."
  (cl-loop for k in keys
           for cell = (assq k alist)
           when (and cell (cdr cell)) return (cdr cell)))

(defun got--field (node field)
  "Return FIELD of the JSON object NODE, using `got-graph-node-keys'."
  (got--get node (alist-get field got-graph-node-keys)))

(defun got--find-list (data keys)
  "Depth-first search DATA for a list of alists under one of KEYS."
  (cond
   ((got--alistp data)
    (or (cl-loop for k in keys
                 for v = (alist-get k data)
                 when (and (listp v) v (got--alistp (car v))) return v)
        (cl-loop for cell in data
                 for r = (got--find-list (cdr cell) keys)
                 when r return r)))
   ((and (listp data) data)
    (cl-loop for el in data
             for r = (got--find-list el keys)
             when r return r))))

(defun got--thoughts (data)
  "Return the list of thought objects found anywhere in DATA, or nil."
  (got--find-list data '(nodes thoughts vertices)))

(defun got--parse-json (string)
  "Parse STRING as JSON into alists and lists, or signal a `user-error'."
  (condition-case err
      (json-parse-string string :object-type 'alist :array-type 'list
                         :null-object nil :false-object nil)
    (error (user-error "Invalid JSON: %s" (error-message-string err)))))

(defun got--embedded-json (data)
  "Find a string inside DATA that is JSON holding thoughts; return it parsed.
MCP tool results often wrap the real payload as a JSON string."
  (cond
   ((stringp data)
    (when (string-match-p "\\`[ \t\n]*[{[]" data)
      (let ((parsed (ignore-errors (got--parse-json data))))
        (and parsed
             (if (got--thoughts parsed) parsed (got--embedded-json parsed))))))
   ((consp data)
    (or (got--embedded-json (car data)) (got--embedded-json (cdr data))))))

(defun got--shape (data)
  "Short description of DATA's structure, for error messages."
  (truncate-string-to-width
   (cond ((got--alistp data)
          (format "object with keys: %s"
                  (mapconcat (lambda (c) (format "%s" (if (consp c) (car c) c)))
                             data ", ")))
         ((listp data) (format "list of %d items" (length data)))
         (t (format "%s" (type-of data))))
   200 nil nil "…"))

(defun got--str (v)
  "Return V as a string: nil becomes empty, non-strings become JSON."
  (cond ((null v) "") ((stringp v) v) (t (json-encode v))))

(defun got--score (v)
  "Return V as a number, or nil when it is not numeric."
  (cond ((numberp v) v)
        ((and (stringp v)
              (string-match-p "\\`[ \t]*-?[0-9]+\\(?:\\.[0-9]+\\)?[ \t]*\\'" v))
         (string-to-number v))))

(defun got--short (text &optional n)
  "First line of TEXT, truncated to N characters.
Only looks at a prefix, and replaces control characters."
  (let* ((head (substring text 0 (min (length text) 200)))
         (head (replace-regexp-in-string "[^\n[:print:]]" " " head))
         (first (car (split-string (string-trim head) "\n" t))))
    (if first (truncate-string-to-width first (or n 50) nil nil "…") "")))

(defun got--unique-id (id byid)
  "Return ID, or ID with a numeric suffix if BYID already has it."
  (let ((new id) (k 1))
    (while (gethash new byid)
      (setq new (format "%s-%d" id (cl-incf k))))
    new))

(defun got--parse-nodes (json)
  "Parse JSON into a list of node plists.
Each has :id :type :content :score :status and :parents.  Ids are made
unique, separate edge lists are merged into :parents, and parents that
are unknown or refer to the node itself are dropped.  Signals a
`user-error' on bad input."
  (let* ((data (let ((d (got--parse-json json)))
                 (if (got--thoughts d) d (or (got--embedded-json d) d))))
         (raw (or (got--thoughts data)
                  (user-error "No thoughts found (%s); adjust `got-graph-node-keys'"
                              (got--shape data))))
         (raw-edges (got--find-list data '(edges links connections)))
         (byid (make-hash-table :test #'equal))
         (i 0)
         nodes)
    (when (> (length raw) got-graph-max-nodes)
      (user-error "Result has %d thoughts; limit is %d (`got-graph-max-nodes')"
                  (length raw) got-graph-max-nodes))
    (dolist (n raw)
      (cl-incf i)
      (let* ((p (got--field n 'parents))
             (id (got--unique-id (got--str (or (got--field n 'id) i)) byid))
             (node (list :id id
                         :type (downcase (got--str (or (got--field n 'type) "thought")))
                         :content (got--str (got--field n 'content))
                         :score (got--score (got--field n 'score))
                         :status (got--str (got--field n 'status))
                         :parents (mapcar #'got--str (if (listp p) p (list p))))))
        (puthash id node byid)
        (push node nodes)))
    (setq nodes (nreverse nodes))
    (dolist (e raw-edges)
      (let ((from (got--str (got--get e '(from source parent src))))
            (to   (got--str (got--get e '(to target child dst)))))
        (when-let* ((n (gethash to byid)))
          (cl-pushnew from (plist-get n :parents) :test #'equal))))
    (dolist (n nodes)
      (let ((id (plist-get n :id)))
        (setf (plist-get n :parents)
              (cl-remove-duplicates
               (cl-remove-if-not (lambda (p) (and (not (equal p id)) (gethash p byid)))
                                 (plist-get n :parents))
               :test #'equal :from-end t))))
    nodes))

(defun got--attach (nodes root-id problem anchor)
  "Return NODES ready to join a graph.
Parentless nodes get a parent: ROOT-ID if non-nil, else ANCHOR if
non-nil.  When ROOT-ID is non-nil a problem node holding PROBLEM is put
first; its own parent is ANCHOR."
  (let ((top (cond (root-id (list root-id)) (anchor (list anchor)))))
    (when top
      (dolist (n nodes)
        (unless (plist-get n :parents)
          (setf (plist-get n :parents) (copy-sequence top)))))
    (if root-id
        (cons (list :id root-id :type "problem" :content problem :score nil
                    :status "" :parents (and anchor (list anchor)))
              nodes)
      nodes)))

(defun got--build-graph (nodes)
  "Build a graph plist from NODES.
Keys: :nodes, :edges ((CHILD . PARENT) ...), :byid (id -> node) and
:children (id -> list of child ids, newest first)."
  (let ((byid (make-hash-table :test #'equal))
        (children (make-hash-table :test #'equal))
        edges)
    (dolist (n nodes)
      (puthash (plist-get n :id) n byid))
    (dolist (n nodes)
      (let ((id (plist-get n :id)))
        (dolist (p (plist-get n :parents))
          (when (gethash p byid)
            (push (cons id p) edges)
            (push id (gethash p children))))))
    (list :nodes nodes :edges (nreverse edges) :byid byid :children children)))

(defun got--parse (json &optional problem)
  "Parse JSON into a graph plist, without touching any file.
With PROBLEM, add a root node that parentless thoughts link to."
  (let ((root (and problem (not (string-blank-p problem)) "got-problem")))
    (got--build-graph (got--attach (got--parse-nodes json) root problem nil))))

;;;; Project-local store

(defconst got-store-version 1
  "Version of the store file format written by this package.")

(defvar got--graph-file nil
  "Store file behind the graph on screen, or nil for an unsaved graph.")

(defvar-local got--detail-id nil
  "Id of the thought shown in this detail buffer.")

(defun got--project-root ()
  "Return the root of the current project, else the nearest git root."
  (or (when-let* ((pr (project-current)))
        (project-root pr))
      (locate-dominating-file default-directory ".git")
      default-directory))

(defun got--store-file ()
  "Return the store file for the current project.
`got-store-file' overrides the default `got-store-file-name' in the
project root, so it can be set per project in `.dir-locals.el'."
  (expand-file-name (or got-store-file got-store-file-name)
                    (got--project-root)))

(defun got--target-file ()
  "Return the store file the current command should act on.
Inside a thought detail buffer that is the file behind the graph on
screen; elsewhere it is the current project's store."
  (or (and (derived-mode-p 'got-detail-mode) got--graph-file)
      (got--store-file)))

(defun got--stored->node (alist)
  "Convert the stored JSON object ALIST into a node plist."
  (let ((id (got--str (alist-get 'id alist))))
    (when (string-empty-p id)
      (user-error "A stored thought has no id"))
    (list :id id
          :type (let ((ty (alist-get 'type alist)))
                  (if (and (stringp ty) (not (string-empty-p ty))) ty "thought"))
          :content (got--str (alist-get 'content alist))
          :score (got--score (alist-get 'score alist))
          :status (got--str (alist-get 'status alist))
          :parents (mapcar #'got--str (alist-get 'parents alist))
          :run (alist-get 'run alist)
          :created (alist-get 'created alist))))

(defun got--node->json (n)
  "Convert the node plist N into an alist for `json-encode'."
  (let ((score (plist-get n :score))
        (status (plist-get n :status))
        (run (plist-get n :run))
        (created (plist-get n :created)))
    `((id . ,(plist-get n :id))
      (type . ,(plist-get n :type))
      (content . ,(plist-get n :content))
      ,@(and score `((score . ,score)))
      ,@(and status (not (string-empty-p status)) `((status . ,status)))
      (parents . ,(vconcat (plist-get n :parents)))
      ,@(and run `((run . ,run)))
      ,@(and created `((created . ,created))))))

(defun got--store-read (file)
  "Read the store FILE into a plist (:runs RUNS :nodes NODES).
A missing or empty file is an empty store.  A file that cannot be read
signals a `user-error' and is never overwritten."
  (if (or (not (file-exists-p file))
          (zerop (file-attribute-size (file-attributes file))))
      (list :runs nil :nodes nil)
    (let* ((text (with-temp-buffer
                   (let ((coding-system-for-read 'utf-8))
                     (insert-file-contents file))
                   (buffer-string)))
           (data (condition-case err
                     (got--parse-json text)
                   (user-error
                    (user-error "%s is not valid JSON (%s); fix or move it, got will not overwrite it"
                                file (error-message-string err)))))
           (version (alist-get 'version data)))
      (unless (got--alistp data)
        (user-error "%s is not a GoT store file" file))
      (when (and (numberp version) (> version got-store-version))
        (user-error "%s has format version %s; this got.el supports up to %s"
                    file version got-store-version))
      (list :runs (alist-get 'runs data)
            :nodes (mapcar #'got--stored->node (alist-get 'nodes data))))))

(defun got--store-write (file store)
  "Write STORE (a plist like `got--store-read' returns) to FILE atomically."
  (let* ((obj `((version . ,got-store-version)
                (runs . ,(vconcat (plist-get store :runs)))
                (nodes . ,(vconcat (mapcar #'got--node->json
                                           (plist-get store :nodes))))))
         (dir (file-name-directory file))
         tmp)
    (make-directory dir t)
    (setq tmp (make-temp-file (expand-file-name ".got-" dir) nil ".tmp"))
    (unwind-protect
        (progn
          (with-temp-buffer
            (insert (json-encode obj))
            (json-pretty-print-buffer)
            (goto-char (point-max))
            (insert "\n")
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region nil nil tmp nil 'silent)))
          (rename-file tmp file t)
          (setq tmp nil))
      (when (and tmp (file-exists-p tmp))
        (delete-file tmp)))))

(defun got--next-run-id (runs)
  "Return the id (rN) for a new run after RUNS."
  (format "r%d"
          (1+ (apply #'max 0
                     (mapcar (lambda (r)
                               (let ((id (alist-get 'id r)))
                                 (if (and (stringp id)
                                          (string-match "\\`r\\([0-9]+\\)\\'" id))
                                     (string-to-number (match-string 1 id))
                                   0)))
                             runs)))))

(defun got--run-nodes (json run problem anchor)
  "Parse JSON into the nodes of run RUN.
Ids are prefixed with the run id so runs never collide.  With PROBLEM
the run gets a root node; ANCHOR is the id of an existing thought that the
run extends."
  (let* ((nodes (got--parse-nodes json))
         (prefix (lambda (id) (concat run "-" id)))
         (root (and problem (not (string-blank-p problem)) (funcall prefix "problem")))
         (created (format-time-string "%FT%T%z")))
    (dolist (n nodes)
      (setf (plist-get n :id) (funcall prefix (plist-get n :id)))
      (setf (plist-get n :parents) (mapcar prefix (plist-get n :parents))))
    (mapcar (lambda (n) (append n (list :run run :created created)))
            (got--attach nodes root problem anchor))))

(defun got--ingest (json &optional problem anchor file)
  "Add the thoughts in JSON to the store as a new run and return the graph.
PROBLEM becomes the run's root node.  ANCHOR is the id of an existing
thought the run extends.  FILE defaults to the project's store.  Nothing
is written unless JSON parses."
  (let* ((file (or file (got--store-file)))
         (store (got--store-read file))
         (old (plist-get store :nodes))
         (run (got--next-run-id (plist-get store :runs))))
    (when (and anchor
               (not (cl-find anchor old :key (lambda (n) (plist-get n :id))
                             :test #'equal)))
      (user-error "Thought %s is not in %s" anchor (abbreviate-file-name file)))
    (let* ((new (got--run-nodes json run problem anchor))
           (entry (delq nil
                        (list (cons 'id run)
                              (and problem (not (string-blank-p problem))
                                   (cons 'problem problem))
                              (cons 'time (format-time-string "%FT%T%z"))
                              (and anchor (cons 'anchor anchor))))))
      (got--store-write file (list :runs (append (plist-get store :runs) (list entry))
                                   :nodes (append old new)))
      (setq got--graph-file file)
      (message "got: saved %d thoughts to %s as run %s"
               (length new) (abbreviate-file-name file) run)
      (got--build-graph (append old new)))))

(defun got--candidates (nodes)
  "Return an alist (LABEL . ID) for completing over NODES."
  (mapcar (lambda (n)
            (cons (replace-regexp-in-string
                   "," ";"
                   (format "%s  [%s] %s" (plist-get n :id) (plist-get n :type)
                           (got--short (plist-get n :content) 50)))
                  (plist-get n :id)))
          nodes))

(defun got--read-type ()
  "Ask for a thought type, defaulting to \"thought\"."
  (completing-read "Type: " (mapcar #'car got-graph-colours) nil nil nil nil "thought"))

;;;###autoload
(defun got-load (&optional file)
  "Load the stored graph of this project and draw it.
With a prefix argument, ask for the store FILE instead."
  (interactive (list (and current-prefix-arg
                          (read-file-name "Load GoT file: " nil nil t))))
  (let* ((file (or file (got--store-file)))
         (nodes (plist-get (got--store-read file) :nodes)))
    (unless nodes
      (user-error "No thoughts stored in %s yet; try M-x got-ask"
                  (abbreviate-file-name file)))
    (message "got: loaded %d thoughts from %s" (length nodes)
             (abbreviate-file-name file))
    (setq got--graph-file file)
    (got-graph-show (got--build-graph nodes))))

;;;###autoload
(defun got-store-open ()
  "Visit the project's store file for hand editing."
  (interactive)
  (find-file (got--store-file)))

;;;###autoload
(defun got-add-thought (content &optional type parents)
  "Add a thought with CONTENT by hand to the project's graph.
TYPE defaults to \"thought\".  PARENTS is a list of ids of stored
thoughts it follows from.  Returns the new id."
  (interactive
   (let* ((cands (got--candidates
                  (plist-get (got--store-read (got--target-file)) :nodes)))
          (content (read-string "Thought: "))
          (type (got--read-type))
          (picked (and cands
                       (completing-read-multiple
                        "Parents (comma-separated, empty for none): "
                        (mapcar #'car cands) nil t))))
     (list content type (delq nil (mapcar (lambda (c) (cdr (assoc c cands))) picked)))))
  (unless got-store-enabled
    (user-error "Persistence is off; set `got-store-enabled'"))
  (when (string-blank-p content)
    (user-error "Give the thought some text"))
  (let* ((file (got--target-file))
         (store (got--store-read file))
         (old (plist-get store :nodes))
         (ids (mapcar (lambda (n) (plist-get n :id)) old))
         (n 1)
         id)
    (dolist (p parents)
      (unless (member p ids)
        (user-error "Parent %s is not in %s" p (abbreviate-file-name file))))
    (while (member (setq id (format "m%d" n)) ids)
      (cl-incf n))
    (let ((new (list :id id :type (or type "thought") :content content
                     :score nil :status "" :parents parents
                     :run "manual" :created (format-time-string "%FT%T%z"))))
      (got--store-write file (list :runs (plist-get store :runs)
                                   :nodes (append old (list new))))
      (setq got--graph-file file)
      (got-graph-show (got--build-graph (append old (list new))))
      id)))

;;;###autoload
(defun got-expand (id &optional question)
  "Ask the server to extend the stored thought ID.
QUESTION defaults to asking for a deeper exploration of the thought.
The new run is linked under ID in the project's graph."
  (interactive
   (let* ((cands (got--candidates
                  (plist-get (got--store-read (got--target-file)) :nodes)))
          (_ (unless cands (user-error "No thoughts stored yet; try M-x got-ask")))
          (label (completing-read "Expand thought: " (mapcar #'car cands) nil t
                                  nil nil (car (rassoc got--detail-id cands)))))
     (list (cdr (assoc label cands))
           (read-string "Question (empty = explore further): "))))
  (let* ((file (got--target-file))
         (node (cl-find id (plist-get (got--store-read file) :nodes)
                        :key (lambda (n) (plist-get n :id)) :test #'equal)))
    (unless node
      (user-error "Thought %s is not in %s" id (abbreviate-file-name file)))
    (got-ask (if (or (null question) (string-blank-p question))
                 (format "Explore and deepen this thought: %s" (plist-get node :content))
               question)
             nil id file)))

;;;; Rendering with graph-fa2

(defvar got--last-graph nil "The most recently parsed graph plist.")
(defvar-local got--freeze-timer nil "Timer that freezes the graph once settled.")

(defun got--radius (n)
  "Node radius for N: the root is large, scored nodes grow with their score.
Scores up to 1 are read as confidences and scaled to 0-10."
  (let* ((s (plist-get n :score))
         (s (and (numberp s) (if (<= s 1) (* 10 s) s))))
    (cond ((equal (plist-get n :type) "problem") 12.0)
          (s (+ 6.0 (* 0.6 (max 0 (min 10 s)))))
          (t 7.0))))

(defun got--fa2-nodes (graph)
  "Convert GRAPH's nodes into graph-fa2 node plists."
  (mapcar (lambda (n)
            (list :id (plist-get n :id)
                  :label (got--short (plist-get n :content) 40)
                  :colour (or (cdr (assoc (plist-get n :type) got-graph-colours))
                              got-graph-default-colour)
                  :radius (got--radius n)))
          (plist-get graph :nodes)))

;;;###autoload
(defun got-graph-show (&optional graph)
  "Render GRAPH (default: the last one) with graph-fa2."
  (interactive)
  (unless (display-graphic-p)
    (user-error "The graph needs a graphical Emacs frame; terminal Emacs cannot show images"))
  (unless (image-type-available-p 'svg)
    (user-error "This Emacs was built without SVG (librsvg) support"))
  (unless (require 'graph-fa2 nil t)
    (user-error "got: cannot load `graph-fa2'; install it first"))
  (let ((graph (or graph got--last-graph)))
    (unless graph (user-error "No graph yet; try M-x got-graph-demo"))
    (unless (plist-get graph :nodes)
      (user-error "No nodes found in the result; see `got-graph-node-keys'"))
    (setq got--last-graph graph)
    (let ((buf (get-buffer-create got-graph-buffer-name))
          (nodes (got--fa2-nodes graph))
          (edges (plist-get graph :edges)))
      (with-current-buffer buf
        (add-hook 'graph-fa2-node-clicked-functions #'got--detail nil t)
        (when got-graph-static
          (setq-local graph-fa2-engine '2d)))
      (pop-to-buffer buf)
      (message "got: drawing %d nodes, %d edges..." (length nodes) (length edges))
      (condition-case err
          (graph-fa2-start buf nodes edges)
        (error (user-error "graph-fa2-start failed: %s" (error-message-string err))))
      (when got-graph-static
        (got--schedule-freeze buf)))))

;;;###autoload
(defun got-graph-doctor ()
  "Report what is wrong when the graph does not appear."
  (interactive)
  (let ((buf (get-buffer got-graph-buffer-name)))
    (with-current-buffer (get-buffer-create "*got-doctor*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert
         (format "Emacs version:         %s\n" emacs-version)
         (format "Graphical frame:       %s\n" (display-graphic-p))
         (format "SVG support:           %s\n" (image-type-available-p 'svg))
         (format "graph-fa2 loadable:    %s\n" (and (locate-library "graph-fa2") t))
         (format "graph-fa2-start bound: %s\n"
                 (progn (require 'graph-fa2 nil t) (fboundp 'graph-fa2-start)))
         (format "Server connected:      %s\n" (and (got--connected-p) t))
         (format "Store file:            %s (%s)\n"
                 (abbreviate-file-name (got--store-file))
                 (cond ((not got-store-enabled) "persistence off")
                       ((file-exists-p (got--store-file)) "exists")
                       (t "not created yet")))
         (format "Last graph:            %s\n"
                 (if got--last-graph
                     (format "%d nodes, %d edges"
                             (length (plist-get got--last-graph :nodes))
                             (length (plist-get got--last-graph :edges)))
                   "none (run M-x got-graph-demo)"))
         (format "Graph buffer:          %s\n"
                 (if buf (format "exists, %d chars, mode %s"
                                 (buffer-size buf)
                                 (buffer-local-value 'major-mode buf))
                   "not created"))
         (format "Player timer:          %s\n"
                 (and buf (boundp 'graph-fa2--player-timer)
                      (and (buffer-local-value 'graph-fa2--player-timer buf) t)))
         (format "Current frame:         %s of %s\n"
                 (and buf (boundp 'graph-fa2--current-frame)
                      (buffer-local-value 'graph-fa2--current-frame buf))
                 (and (boundp 'graph-fa2-simulation-frames) graph-fa2-simulation-frames))
         (format "Freeze enabled:        %s\n" got-graph-static))
        (when (fboundp 'graph-fa2-start)
          (insert (format "graph-fa2-start args:  %S\n"
                          (help-function-arglist 'graph-fa2-start t)))))
      (special-mode)
      (pop-to-buffer (current-buffer)))))

;;;; Freezing

(defun got--schedule-freeze (buf)
  "Freeze BUF once playback has reached the last simulation frame.
Polls cheaply and gives up after `got-graph-freeze-timeout' seconds, so
the graph is never frozen before its frames exist."
  (with-current-buffer buf
    (when (timerp got--freeze-timer)
      (cancel-timer got--freeze-timer))
    (let ((deadline (+ (float-time) got-graph-freeze-timeout))
          (peak 0)
          timer)
      (setq timer
            (run-at-time
             1 0.25
             (lambda ()
               (cond
                ((not (buffer-live-p buf)) (cancel-timer timer))
                ((> (float-time) deadline)
                 (cancel-timer timer)
                 (message "got: graph never reached its last frame; left running"))
                (t
                 (let ((frame (buffer-local-value 'graph-fa2--current-frame buf))
                       (last (1- graph-fa2-simulation-frames)))
                   (when (numberp frame)
                     (if (or (>= frame (* 0.97 last)) (< frame peak))
                         (progn (cancel-timer timer) (got--freeze buf))
                       (setq peak (max peak frame))))))))))
      (setq got--freeze-timer timer))))

(defun got--freeze (buf)
  "Stop playback and background simulation in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      ;; Leave `graph-fa2--player-timer' set but cancelled, so graph-fa2 still
      ;; treats the graph as playing: dragging stays click-only and cannot
      ;; restart the simulation.
      (when (timerp graph-fa2--player-timer)
        (cancel-timer graph-fa2--player-timer))
      (ignore-errors
        (let ((bg (and graph-fa2-ctx (graph-fa2-ctx-bg-timer graph-fa2-ctx))))
          (when (timerp bg) (cancel-timer bg))))
      (when (timerp got--freeze-timer)
        (cancel-timer got--freeze-timer))
      (message "got: layout frozen"))))

;;;###autoload
(defun got-graph-freeze ()
  "Freeze the graph now, wherever the layout currently is."
  (interactive)
  (if-let* ((buf (get-buffer got-graph-buffer-name)))
      (got--freeze buf)
    (user-error "No graph buffer")))

;;;; Detail buffer (opened on click)

(defvar-keymap got-detail-mode-map
  :doc "Keymap for `got-detail-mode'."
  :parent special-mode-map
  "e" #'got-detail-expand
  "c" #'got-detail-add-child)

(define-derived-mode got-detail-mode special-mode "GoT-Thought"
  "Major mode for viewing one thought.
\\<got-detail-mode-map>\\[got-detail-expand] asks the server to extend
the thought, \\[got-detail-add-child] adds a child thought by hand.")

(defun got-detail-expand ()
  "Ask the server to extend the thought shown in this buffer."
  (interactive)
  (unless got--detail-id (user-error "No thought here"))
  (got-expand got--detail-id (read-string "Question (empty = explore further): ")))

(defun got-detail-add-child ()
  "Add a child thought by hand under the thought shown in this buffer."
  (interactive)
  (unless got--detail-id (user-error "No thought here"))
  (got-add-thought (read-string "Child thought: ") (got--read-type)
                   (list got--detail-id)))

(defun got--link (id)
  "Insert a button that opens the detail view of thought ID."
  (insert-text-button
   (got--short (plist-get (gethash id (plist-get got--last-graph :byid)) :content) 60)
   'action (lambda (_) (got--detail id))
   'follow-link t)
  (insert "\n"))

(defun got--detail (id)
  "Show the thought ID in a side buffer."
  (let ((n (gethash id (plist-get got--last-graph :byid))))
    (if (not n)
        (message "got: unknown node %s" id)
      (let ((children (reverse (gethash id (plist-get got--last-graph :children)))))
        (with-current-buffer (get-buffer-create "*got-thought*")
          (got-detail-mode)
          (setq got--detail-id id)
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Type:   %s\nScore:  %s\n"
                            (plist-get n :type)
                            (or (plist-get n :score) "-")))
            (unless (string-empty-p (plist-get n :status))
              (insert (format "Status: %s\n" (plist-get n :status))))
            (when (plist-get n :run)
              (insert (format "Run:    %s\n" (plist-get n :run))))
            (insert "\n" (plist-get n :content) "\n")
            (when (plist-get n :parents)
              (insert "\nParents:\n")
              (mapc #'got--link (plist-get n :parents)))
            (when children
              (insert "\nChildren:\n")
              (mapc #'got--link children))
            (when got--graph-file
              (insert "\n[e] expand with the server   [c] add a child thought\n"))
            (goto-char (point-min)))
          (display-buffer (current-buffer)
                          '((display-buffer-reuse-window display-buffer-pop-up-window)
                            (inhibit-same-window . t))))))))

;;;; Importing results

;;;###autoload
(defun got-graph-import-string (json &optional problem anchor file)
  "Parse JSON and draw it, saving it to the project store.
PROBLEM is an optional root node.  ANCHOR is the id of a stored thought
this result extends.  FILE overrides the store file.  With
`got-store-enabled' nil nothing is saved."
  (got-graph-show (if got-store-enabled
                      (got--ingest json problem anchor file)
                    (setq got--graph-file nil)
                    (got--parse json problem))))

;;;###autoload
(defun got-graph-import-region (beg end &optional problem)
  "Add the JSON between BEG and END to the graph, with PROBLEM as its root."
  (interactive "r\nsProblem (optional, adds a root node): ")
  (got-graph-import-string (buffer-substring-no-properties beg end) problem))

;;;###autoload
(defun got-graph-import-file (file)
  "Add the JSON in FILE to the graph."
  (interactive "fJSON file: ")
  (got-graph-import-string
   (with-temp-buffer (insert-file-contents file) (buffer-string))))

;;;; Asking through gptel

(defvar-local got--problem nil "Text of the last `got-ask' in this buffer.")
(defvar-local got--anchor nil
  "Id of the stored thought the last `got-ask' extends, or nil.")
(defvar-local got--ask-file nil
  "Store file chosen when the last `got-ask' was issued.")
(defvar-local got--last-json-hash nil "Hash of the JSON block last saved.")

(defun got--json-block-after (beg)
  "Return the last ```json block at or after BEG (whole buffer if nil)."
  (save-excursion
    (save-restriction
      (narrow-to-region (if beg (max (point-min) (min beg (point-max))) (point-min))
                        (point-max))
      (goto-char (point-max))
      (when (re-search-backward "```json[ \t]*\n" nil t)
        (let ((start (match-end 0)))
          (when (re-search-forward "\n[ \t]*```" nil t)
            (buffer-substring-no-properties start (match-beginning 0))))))))

;;;###autoload
(defun got-graph-from-buffer (&optional beg _end)
  "Save and draw the last ```json block in this buffer.
Runs automatically after a `got-ask' response, searching only the new
response (BEG..END from gptel).  Call it by hand to retry on the whole
buffer; a response that was already saved is just shown again."
  (interactive)
  (let* ((manual (called-interactively-p 'any))
         (json (or (and (not manual) (number-or-marker-p beg)
                        (got--json-block-after beg))
                   (got--json-block-after nil)))
         (hash (and json (secure-hash 'sha1 json))))
    (cond
     ((null json)
      (when manual (user-error "No ```json block found in this buffer")))
     ((equal hash got--last-json-hash)
      (when manual
        (message "got: this response is already saved")
        (got-load got--ask-file)))
     (t
      (condition-case err
          (progn
            (got-graph-import-string json got--problem got--anchor got--ask-file)
            (setq got--last-json-hash hash))
        (error (message "got: could not graph the response: %s"
                        (error-message-string err))))))))

(defun got--ask (problem tool &optional anchor file)
  "Send PROBLEM to gptel, asking it to call server TOOL.
ANCHOR is the id of a stored thought the result extends and FILE the
store to save it in.  The server must be ready."
  (let ((buf (gptel "*GoT*")))
    (with-current-buffer buf
      (setq-local gptel-use-tools t)
      (setq-local gptel-tools (got--tools))
      (setq-local got--problem problem)
      (setq-local got--anchor anchor)
      (setq-local got--ask-file file)
      (setq-local got--last-json-hash nil)
      (add-hook 'gptel-post-response-functions #'got-graph-from-buffer nil t)
      (goto-char (point-max))
      (insert (format got-ask-prompt tool problem))
      (gptel-send))
    (pop-to-buffer buf)))

;;;###autoload
(defun got-ask (problem &optional tool anchor file)
  "Ask gptel to answer PROBLEM using the GoT server's TOOL.
TOOL defaults to the first entry of `got-tools-list'; with a prefix
argument you are asked which one.  The result is saved to the project's
store; ANCHOR is the id of a stored thought it extends, and FILE
overrides the store file.  Starts the server on first use and continues
once it is ready."
  (interactive
   (list (read-string "Question or claim: ")
         (and current-prefix-arg
              (completing-read "Tool: " got-tools-list nil nil))))
  (when (string-blank-p problem)
    (user-error "Give a question or claim to reason about"))
  (let ((tool (or tool (car got-tools-list)))
        (file (or file (got--store-file))))
    (got--when-ready #'got--tools (lambda () (got--ask problem tool anchor file)))))

;;;; Demo

(defconst got--demo-json
  "{\"nodes\":[
 {\"id\":\"g1\",\"operation\":\"generate\",\"content\":\"Pivot to enterprise customers\",\"score\":7.5},
 {\"id\":\"g2\",\"operation\":\"generate\",\"content\":\"Stay the course and cut costs\",\"score\":5.0},
 {\"id\":\"g3\",\"operation\":\"generate\",\"content\":\"Open-source the core, sell hosting\",\"score\":8.0},
 {\"id\":\"s1\",\"operation\":\"select\",\"content\":\"Keep the two best-scoring ideas\",\"parents\":[\"g1\",\"g3\"]},
 {\"id\":\"a1\",\"operation\":\"aggregate\",\"content\":\"Open-source core, then upsell enterprise support\",\"parents\":[\"g1\",\"g3\"],\"score\":9.0},
 {\"id\":\"r1\",\"operation\":\"refine\",\"content\":\"Add a 12-month runway plan to the combined strategy\",\"parents\":[\"a1\"],\"score\":9.3}
]}"
  "Sample graph for `got-graph-demo'.  Not real server output.")

;;;###autoload
(defun got-graph-demo ()
  "Render a built-in sample graph.  Needs no server and saves nothing."
  (interactive)
  (setq got--graph-file nil)
  (got-graph-show (got--parse got--demo-json "Startup strategy under limited runway")))

(provide 'got)
;;; got.el ends here
