;;; gptel-eww-search.el --- gptel web search via eww with automatic citation highlighting -*- lexical-binding: t; -*-

;; Requires: Emacs 28.1+, gptel (with tool support), eww/shr (built in).

;;; Commentary:

;; Three gptel tools that use eww as the web client, plus a post-response
;; hook that links the citations in the answer:
;;
;;   eww_search   run a web search, return titles / URLs / snippets
;;   eww_read     load a page in eww and return its text.  Every paragraph
;;                is prefixed with an ID such as [2.5] (source 2, paragraph 5).
;;   eww_cite     the model delivers its citation data: a list of
;;                {id, quote} pairs.  Each verbatim quote is highlighted
;;                in the eww buffer of the page right away, and the model
;;                gets back which quotes were found.
;;
;; The model then puts the IDs after the claims it takes from a page,
;; e.g. "The bridge opened in 1932 [2.5]."  When the response is complete,
;; `gptel-eww--process-response' (on `gptel-post-response-functions'):
;;
;;   1. finds every [S.P] citation in the response,
;;   2. looks up the highlights that eww_cite made for that paragraph; if
;;      the model delivered none, it highlights the sentence that best
;;      matches the claim's wording instead (the whole paragraph if
;;      nothing matches well), and
;;   3. turns the citation into a button: RET / mouse-1 jumps to the
;;      highlighted text in eww.  Hovering shows the source title and URL.
;;
;; This only needs function calling, so it works with any OpenAI-compatible
;; backend that supports tools.
;;
;; Setup:
;;
;;   (require 'gptel-eww-search)
;;   (gptel-eww-register)   ; adds the tools and the hook
;;
;; Then enable the "web" tools in the gptel menu (C-c RET, then t).
;;
;; Commands:
;;   M-x gptel-eww-show-sources       jump to any page that was read
;;   M-x gptel-eww-clear-highlights   remove all highlights
;;   M-x gptel-eww-kill-sources       kill all source buffers

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'button)
(require 'eww)
(require 'gptel)

;;;; Customization

(defgroup gptel-eww nil
  "gptel tools that browse with eww."
  :group 'gptel)

(defcustom gptel-eww-search-url "https://html.duckduckgo.com/html/?q=%s"
  "Search URL template.  %s is replaced by the hex-encoded query.
Any engine with a plain-HTML results page works."
  :type 'string)

(defcustom gptel-eww-max-results 8
  "Maximum number of search results returned to the model."
  :type 'integer)

(defcustom gptel-eww-max-chars 12000
  "Maximum number of characters of page text returned per eww_read call."
  :type 'integer)

(defcustom gptel-eww-timeout 25
  "Seconds to wait for eww to finish loading a page."
  :type 'number)

(defcustom gptel-eww-readable t
  "Non-nil means switch pages to eww's readable mode before reading them.
This strips navigation and boilerplate, so paragraph IDs point at content."
  :type 'boolean)

(defcustom gptel-eww-chunk-size 350
  "Target size in characters of one citable paragraph."
  :type 'integer)

(defcustom gptel-eww-min-chunk 40
  "Fragments shorter than this are merged into the following paragraph."
  :type 'integer)

(defcustom gptel-eww-match-threshold 0.34
  "Minimum word overlap (0-1) for highlighting a single sentence.
Below it, the whole cited paragraph is highlighted instead."
  :type 'float)

(defcustom gptel-eww-auto-display nil
  "Non-nil means pop up the first cited page when a response finishes.
Otherwise highlights are applied silently and the citation buttons in the
gptel buffer take you there."
  :type 'boolean)

(defface gptel-eww-cite-face
  '((((background dark))  :background "#5c4b00" :foreground "#fff3b0" :extend nil)
    (((background light)) :background "#fff176" :foreground "#000000" :extend nil))
  "Face for text cited by the model.")

(defface gptel-eww-cite-link-face
  '((t :inherit link :weight bold))
  "Face for citation buttons in the gptel buffer.")

;;;; Sources

(cl-defstruct (gptel-eww-source (:constructor gptel-eww-source--make)
                                (:copier nil))
  n url title buffer chunks)

(defvar gptel-eww--sources nil
  "Pages read so far, newest first.  IDs stay valid for the whole session.")

(defvar gptel-eww--counter 0
  "Number of the most recently registered source.")

(defun gptel-eww--live-source (url)
  "Return the live source for URL, if it was already read."
  (cl-find-if (lambda (s)
                (and (equal (gptel-eww-source-url s) url)
                     (buffer-live-p (gptel-eww-source-buffer s))))
              gptel-eww--sources))

(defun gptel-eww--source-by-n (n)
  "Return the source numbered N."
  (cl-find n gptel-eww--sources :key #'gptel-eww-source-n))

;;;; Page loading

(defun gptel-eww--fetch (url callback &optional readable)
  "Load URL in a dedicated eww buffer and call CALLBACK with that buffer.
CALLBACK receives nil if the page did not load within
`gptel-eww-timeout' seconds.  With READABLE, switch to readable mode."
  (let* ((buf (generate-new-buffer "*gptel-eww*"))
         (done nil) (timer nil) (hook nil))
    (setq hook
          (lambda ()
            (unless done
              (setq done t)
              (when (timerp timer) (cancel-timer timer))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (remove-hook 'eww-after-render-hook hook t)
                  (when readable (ignore-errors (eww-readable)))))
              (funcall callback buf))))
    (setq timer
          (run-at-time
           gptel-eww-timeout nil
           (lambda ()
             (unless done
               (setq done t)
               (when (buffer-live-p buf)
                 (with-current-buffer buf
                   (remove-hook 'eww-after-render-hook hook t))
                 (kill-buffer buf))
               (funcall callback nil)))))
    (condition-case err
        ;; `eww' reuses the current buffer when it is in `eww-mode' and
        ;; pops it up; `save-window-excursion' keeps your layout intact.
        (save-window-excursion
          (with-current-buffer buf
            (eww-mode)
            (add-hook 'eww-after-render-hook hook nil t)
            (eww url)))
      (error
       (unless done
         (setq done t)
         (when (timerp timer) (cancel-timer timer))
         (message "gptel-eww: %s" (error-message-string err))
         (funcall callback nil))))))

;;;; Search

(defun gptel-eww--normalize-link (link)
  "Return the real target of LINK, unwrapping search-engine redirects.
Return nil for links that are not external http(s) URLs."
  (when (stringp link)
    (let ((l (if (string-prefix-p "//" link) (concat "https:" link) link)))
      (cond
       ((string-match "[?&]uddg=\\([^&]+\\)" l)
        (url-unhex-string (match-string 1 l)))
       ((and (string-match-p "\\`https?://" l)
             (not (string-match-p
                   "\\`https?://\\(?:[a-z.]*\\)?duckduckgo\\.com" l)))
        l)))))

(defun gptel-eww--parse-results (buf)
  "Extract (TITLE URL SNIPPET) triples from the search page in BUF."
  (with-current-buffer buf
    (let ((pos (point-min)) (table nil) (order nil))
      (while (< pos (point-max))
        (let* ((link (get-text-property pos 'shr-url))
               (end (or (next-single-property-change
                         pos 'shr-url nil (point-max))
                        (point-max))))
          (when link
            (let ((url (gptel-eww--normalize-link link))
                  (text (string-trim
                         (replace-regexp-in-string
                          "[ \t\n\u00a0]+" " "
                          (buffer-substring-no-properties pos end)))))
              (when (and url (not (string-empty-p text)))
                (let ((entry (assoc url table)))
                  (if (null entry)
                      (progn (push (list url text nil) table)
                             (push url order))
                    ;; Further links to the same URL: the longest text is
                    ;; the snippet (the first one is the title).
                    (when (> (length text) (length (or (nth 2 entry) "")))
                      (setf (nth 2 entry) text)))))))
          (setq pos end)))
      (mapcar (lambda (url)
                (let ((e (assoc url table)))
                  (list (nth 1 e) (nth 0 e) (nth 2 e))))
              (seq-take (nreverse order) gptel-eww-max-results)))))

(defun gptel-eww--search (callback query)
  "gptel tool body: search for QUERY, report results through CALLBACK."
  (gptel-eww--fetch
   (format gptel-eww-search-url (url-hexify-string query))
   (lambda (buf)
     (if (null buf)
         (funcall callback "Error: the search page did not load in time.")
       (let ((results (gptel-eww--parse-results buf)))
         (kill-buffer buf)
         (funcall
          callback
          (if (null results)
              "No results found (the search engine may have blocked the request)."
            (string-join
             (cl-loop for (title url snippet) in results
                      for i from 1
                      collect (format "%d. %s\n   %s%s" i title url
                                      (if snippet (concat "\n   " snippet) "")))
             "\n\n"))))))))

;;;; Reading: split pages into citable paragraphs

(defun gptel-eww--chunk-buffer (buf)
  "Split the rendered page in BUF into paragraphs.
Return a list of (BEG-MARKER . END-MARKER) in buffer order."
  (with-current-buffer buf
    (save-excursion
      (goto-char (point-min))
      (let (chunks beg end (len 0))
        (cl-flet ((flush (force)
                    (when (and beg (or force (>= len gptel-eww-min-chunk)))
                      (push (cons (copy-marker beg) (copy-marker end)) chunks)
                      (setq beg nil end nil len 0))))
          (while (not (eobp))
            (let* ((lb (line-beginning-position))
                   (le (line-end-position))
                   (tb (save-excursion
                         (goto-char lb) (skip-chars-forward " \t\u00a0" le)
                         (point)))
                   (te (save-excursion
                         (goto-char le) (skip-chars-backward " \t\u00a0" lb)
                         (point))))
              (if (>= tb te)
                  (flush nil)
                (unless beg (setq beg tb))
                (setq end te)
                (cl-incf len (- te tb))
                (when (or (and (>= len gptel-eww-chunk-size)
                               (memq (char-before te) '(?. ?! ?? ?\" ?” ?\))))
                          (>= len (* 2 gptel-eww-chunk-size)))
                  (flush t))))
            (forward-line 1))
          (flush nil))
        (nreverse chunks)))))

(defun gptel-eww--register (buf url)
  "Register the page in BUF, loaded from URL, as a new source."
  (let ((src (gptel-eww-source--make
              :n (cl-incf gptel-eww--counter)
              :url url
              :title (with-current-buffer buf (plist-get eww-data :title))
              :buffer buf
              :chunks (vconcat (gptel-eww--chunk-buffer buf)))))
    (push src gptel-eww--sources)
    src))

(defun gptel-eww--chunk-text (src i)
  "Return the whitespace-normalized text of paragraph I (0-based) of SRC."
  (let ((c (aref (gptel-eww-source-chunks src) i)))
    (with-current-buffer (gptel-eww-source-buffer src)
      (replace-regexp-in-string
       "[ \t\n\u00a0]+" " "
       (buffer-substring-no-properties (car c) (cdr c))))))

(defun gptel-eww--render-source (src start)
  "Return the text of SRC for the model, from paragraph START (1-based)."
  (let* ((n (gptel-eww-source-n src))
         (total (length (gptel-eww-source-chunks src)))
         (i (1- start)) (size 0) (out nil))
    (while (and (< i total) (< size gptel-eww-max-chars))
      (let ((line (format "[%d.%d] %s" n (1+ i) (gptel-eww--chunk-text src i))))
        (push line out)
        (cl-incf size (length line))
        (cl-incf i)))
    (if (null out)
        (format "Source %d (%s) has no readable text at paragraph %d."
                n (gptel-eww-source-url src) start)
      (concat
       (format "Source %d: %s\nURL: %s\nParagraphs %d-%d of %d.\n"
               n (or (gptel-eww-source-title src) "")
               (gptel-eww-source-url src) start i total)
       "Citing: before your final answer, call eww_cite once with the ID "
       "and a short verbatim quote for each fact you use from this page.  "
       "In the answer, put the paragraph ID in square brackets right after "
       "the claim, e.g. \"It opened in 1932 [" (number-to-string n) ".4].\"  "
       "Use only IDs shown here.\n\n"
       (string-join (nreverse out) "\n\n")
       (when (< i total)
         (format "\n\n[... %d more paragraphs; call eww_read again with start=%d ...]"
                 (- total i) (1+ i)))))))

(defun gptel-eww--read (callback url &optional start)
  "gptel tool body: load URL and report numbered paragraphs via CALLBACK."
  (let ((start (max 1 (if (numberp start) (truncate start) 1)))
        (src (gptel-eww--live-source url)))
    (if src
        (funcall callback (gptel-eww--render-source src start))
      (gptel-eww--fetch
       url
       (lambda (buf)
         (funcall callback
                  (if buf
                      (gptel-eww--render-source
                       (gptel-eww--register buf url) start)
                    "Error: the page did not load in time.")))
       gptel-eww-readable))))

;;;; Matching a claim to the cited text

(defconst gptel-eww--stop
  '("that" "this" "with" "from" "have" "were" "their" "which" "about" "been"
    "also" "they" "than" "then" "there" "these" "those" "would" "could"
    "into" "such" "other" "will" "when" "what" "your" "more" "some")
  "Words ignored when comparing a claim with source text.")

(defun gptel-eww--tokens (s)
  "Return the distinct content words of S, lowercased."
  (let (r)
    (dolist (w (split-string (downcase s) "[^[:alnum:]]+" t))
      (when (and (or (>= (length w) 4) (string-match-p "[0-9]" w))
                 (not (member w gptel-eww--stop)))
        (cl-pushnew w r :test #'equal)))
    r))

(defun gptel-eww--overlap (a b)
  "Word overlap between token lists A and B, from 0 to 1."
  (if (or (null a) (null b))
      0.0
    (/ (float (length (cl-intersection a b :test #'equal)))
       (min (length a) (length b)))))

(defun gptel-eww--sentence-spans (beg end)
  "Split BEG..END of the current buffer into sentence spans (BEG . END)."
  (save-excursion
    (goto-char beg)
    (let ((s beg) spans)
      (while (re-search-forward "[.!?][\"'”’)]*[ \t\n\u00a0]+" end t)
        (push (cons s (1+ (match-beginning 0))) spans)
        (setq s (match-end 0)))
      (when (< s end) (push (cons s end) spans))
      (nreverse spans))))

(defun gptel-eww--add-overlay (beg end)
  "Highlight BEG..END in the current buffer, once."
  (unless (cl-some (lambda (ov)
                     (and (overlay-get ov 'gptel-eww-cite)
                          (= (overlay-start ov) beg)
                          (= (overlay-end ov) end)))
                   (overlays-in beg end))
    (let ((ov (make-overlay beg end)))
      (overlay-put ov 'gptel-eww-cite t)
      (overlay-put ov 'face 'gptel-eww-cite-face)
      (overlay-put ov 'priority 100)
      (overlay-put ov 'help-echo "Cited by gptel"))))

(defun gptel-eww--highlight (src idx claim)
  "Highlight, in SRC, the text of paragraph IDX that backs CLAIM.
Return the start position of the highlight, or nil."
  (let ((chunks (gptel-eww-source-chunks src))
        (buf (gptel-eww-source-buffer src)))
    (when (and (buffer-live-p buf) (<= 1 idx (length chunks)))
      (with-current-buffer buf
        (let* ((c (aref chunks (1- idx)))
               (cb (marker-position (car c)))
               (ce (marker-position (cdr c)))
               (ctoks (gptel-eww--tokens claim))
               (best nil) (best-score 0.0))
          (dolist (sp (gptel-eww--sentence-spans cb ce))
            (let* ((st (gptel-eww--tokens
                        (buffer-substring-no-properties (car sp) (cdr sp))))
                   (hits (length (cl-intersection ctoks st :test #'equal)))
                   (score (gptel-eww--overlap ctoks st)))
              (when (and (>= hits 2) (> score best-score))
                (setq best sp best-score score))))
          (let ((span (if (and best (>= best-score gptel-eww-match-threshold))
                          best
                        (cons cb ce))))
            (gptel-eww--add-overlay (car span) (cdr span))
            (car span)))))))

;;;; Citation data delivered by the model (eww_cite)

(defvar gptel-eww--cited (make-hash-table :test #'equal)
  "Map from \"S.P\" to the (BUFFER . POS) highlights made by eww_cite.")

(defun gptel-eww--field (obj key)
  "Get field KEY (a string) from the JSON object OBJ, however it was parsed."
  (cond ((hash-table-p obj) (gethash key obj))
        ((and (consp obj) (keywordp (car obj)))
         (plist-get obj (intern (concat ":" key))))
        ((consp obj)
         (cdr (or (assoc key obj) (assoc (intern key) obj))))))

(defun gptel-eww--clean-quote (text)
  "Strip surrounding quote marks and ellipses from the model's quote TEXT."
  (string-trim text "[ \t\n\"“”'…]+" "[ \t\n\"“”'…]+"))

(defun gptel-eww--quote-regexp (text)
  "Build a regexp matching TEXT regardless of whitespace and quote style."
  (mapconcat
   (lambda (w)
     (let ((r (regexp-quote w)))
       (setq r (replace-regexp-in-string "['’‘]" "['’‘]" r t t))
       (replace-regexp-in-string "[\"“”]" "[\"“”]" r t t)))
   (split-string text "[ \t\n ]+" t)
   "[ \t\n ]+"))

(defun gptel-eww--cite-one (id-str text)
  "Highlight the quote TEXT in the paragraph named by ID-STR.
Return a one-line status string for the model."
  (if (not (and (stringp id-str)
                (string-match "\\([0-9]+\\)\\.\\([0-9]+\\)" id-str)))
      (format "- %S: invalid id (use the S.P ids shown by eww_read)" id-str)
    (let* ((n (string-to-number (match-string 1 id-str)))
           (idx (string-to-number (match-string 2 id-str)))
           (key (format "%d.%d" n idx))
           (src (gptel-eww--source-by-n n))
           (rx (and (stringp text)
                    (gptel-eww--quote-regexp (gptel-eww--clean-quote text)))))
      (cond
       ((not (and src (buffer-live-p (gptel-eww-source-buffer src))))
        (format "- [%s]: unknown source, read the page with eww_read first" key))
       ((not (<= 1 idx (length (gptel-eww-source-chunks src))))
        (format "- [%s]: no such paragraph" key))
       ((or (null rx) (string-empty-p rx))
        (format "- [%s]: empty quote" key))
       (t
        (with-current-buffer (gptel-eww-source-buffer src)
          (let ((c (aref (gptel-eww-source-chunks src) (1- idx)))
                (case-fold-search t)
                hit where)
            (save-excursion
              (goto-char (car c))
              (when (re-search-forward rx (cdr c) t)
                (setq hit (cons (match-beginning 0) (match-end 0))
                      where 'here))
              (unless hit
                (goto-char (point-min))
                (when (re-search-forward rx nil t)
                  (setq hit (cons (match-beginning 0) (match-end 0))
                        where 'elsewhere))))
            (if (null hit)
                (format (concat "- [%s]: quote not found verbatim; copy the "
                                "exact wording from the eww_read output and "
                                "call eww_cite again for it")
                        key)
              (gptel-eww--add-overlay (car hit) (cdr hit))
              (cl-pushnew (cons (current-buffer) (car hit))
                          (gethash key gptel-eww--cited) :test #'equal)
              (format "- [%s]: highlighted%s" key
                      (if (eq where 'elsewhere)
                          " (the quote is in a different paragraph than the id)"
                        ""))))))))))

(defun gptel-eww--cite (citations)
  "gptel tool body: highlight the quotes in CITATIONS, a list of {id, quote}."
  (let ((items (append citations nil)))
    (if (null items)
        "Error: pass a non-empty list of {id, quote} objects."
      (concat
       "Citation highlights:\n"
       (string-join
        (mapcar (lambda (o)
                  (gptel-eww--cite-one (gptel-eww--field o "id")
                                       (gptel-eww--field o "quote")))
                items)
        "\n")
       "\nNow write your answer and put the [S.P] ids after the claims."))))

;;;; Citations in the gptel buffer

(defconst gptel-eww--cite-rx
  "\\[\\([0-9]+\\.[0-9]+\\(?:[ ]*[,;][ ]*[0-9]+\\.[0-9]+\\)*\\)\\]"
  "Matches citations such as [2.5] or [2.5, 2.6].")

(defun gptel-eww--parse-ids (str)
  "Parse \"2.5, 2.6\" into ((2 . 5) (2 . 6))."
  (mapcar (lambda (s)
            (let ((p (split-string s "\\.")))
              (cons (string-to-number (car p)) (string-to-number (cadr p)))))
          (split-string str "[ ,;]+" t)))

(defun gptel-eww--claim-before (beg pos)
  "Return the claim that the citation at POS supports (text after BEG)."
  (let* ((raw (buffer-substring-no-properties (max beg (- pos 400)) pos))
         (p 0) (cut nil))
    (while (string-match gptel-eww--cite-rx raw p)
      (setq cut (match-end 0) p cut))
    (let* ((tail (if cut (substring raw cut) raw))
           (text (if (string-blank-p tail)
                     (replace-regexp-in-string gptel-eww--cite-rx "" raw)
                   tail))
           (sents (split-string
                   text "\\(?:[.!?][ \t]+\\|\n+\\)" t "[ \t]+")))
      (or (car (last sents)) ""))))

(defun gptel-eww--goto (buf pos)
  "Show BUF with POS in view."
  (if (not (buffer-live-p buf))
      (message "gptel-eww: the source buffer is gone")
    (let ((win (display-buffer buf)))
      (when (window-live-p win)
        (set-window-point win pos)
        (with-selected-window win
          (goto-char pos)
          (recenter 3))))))

(defun gptel-eww--follow (button)
  "Jump to the cited text for BUTTON; repeat to cycle through its sources."
  (let* ((targets (button-get button 'gptel-eww-targets))
         (i (or (button-get button 'gptel-eww-index) 0))
         (tg (nth (mod i (length targets)) targets)))
    (button-put button 'gptel-eww-index (1+ i))
    (gptel-eww--goto (car tg) (cdr tg))))

(define-button-type 'gptel-eww-cite
  'face 'gptel-eww-cite-link-face
  'follow-link t
  'action #'gptel-eww--follow)

(defun gptel-eww--process-response (beg end)
  "Highlight and link the citations in the response between BEG and END.
Meant for `gptel-post-response-functions'."
  (when gptel-eww--sources
    (let ((end (copy-marker end)) (first nil))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward gptel-eww--cite-rx end t)
          (let* ((mb (match-beginning 0))
                 (me (match-end 0))
                 (ids (gptel-eww--parse-ids (match-string 1)))
                 (claim (gptel-eww--claim-before beg mb))
                 (targets nil) (labels nil))
            (dolist (id ids)
              (let ((src (gptel-eww--source-by-n (car id))))
                (when src
                  ;; Prefer the exact quotes the model delivered via
                  ;; eww_cite; fall back to matching the claim's wording.
                  (let* ((cited (cl-remove-if-not
                                 (lambda (tg) (buffer-live-p (car tg)))
                                 (gethash (format "%d.%d" (car id) (cdr id))
                                          gptel-eww--cited)))
                         (tgs (or (reverse cited)
                                  (let ((pos (gptel-eww--highlight
                                              src (cdr id) claim)))
                                    (and pos
                                         (list (cons (gptel-eww-source-buffer src)
                                                     pos)))))))
                    (when tgs
                      (dolist (tg tgs) (push tg targets))
                      (push (format "[%d.%d] %s <%s>" (car id) (cdr id)
                                    (or (gptel-eww-source-title src) "")
                                    (gptel-eww-source-url src))
                            labels))))))
            (when targets
              (setq targets (nreverse targets))
              (unless first (setq first (car targets)))
              (make-button mb me
                           'type 'gptel-eww-cite
                           'gptel-eww-targets targets
                           'help-echo (string-join (nreverse labels) "\n"))))))
      (set-marker end nil)
      (when (and first gptel-eww-auto-display)
        (gptel-eww--goto (car first) (cdr first))))))

;;;; Tools

(defvar gptel-eww-tools
  (list
   (gptel-make-tool
    :name "eww_search"
    :category "web"
    :async t
    :function #'gptel-eww--search
    :description
    "Search the web.  Returns numbered results with title, URL and a short snippet.  Follow up with eww_read on the most relevant URLs."
    :args '((:name "query" :type string
             :description "Search query, ideally a few focused keywords.")))
   (gptel-make-tool
    :name "eww_read"
    :category "web"
    :async t
    :function #'gptel-eww--read
    :description
    "Load a web page in the eww browser and return its text as paragraphs, each prefixed with a citation ID like [2.5].  When your answer uses information from a page, first call eww_cite with the IDs and short verbatim quotes you rely on, then put the matching ID(s) in square brackets directly after each claim in your answer, e.g. \"The bridge opened in 1932 [2.5].\"  Never invent IDs."
    :args '((:name "url" :type string
             :description "Full http(s) URL, e.g. one returned by eww_search.")
            (:name "start" :type integer :optional t
             :description "Paragraph number to start from, for reading long pages in several calls.  Default 1.")))
   (gptel-make-tool
    :name "eww_cite"
    :category "web"
    :function #'gptel-eww--cite
    :description
    "Register the exact passages your answer is based on, so the user's editor can highlight them on the source pages.  Call this once, after reading pages with eww_read and before writing the final answer.  Then cite the same IDs as [S.P] after the matching claims in your answer."
    :args '((:name "citations" :type array
             :description "One entry per fact used.  Each has the paragraph ID from eww_read (like 2.5) and a short verbatim quote (one sentence or less) copied exactly from that paragraph."
             :items (:type object
                     :properties (:id (:type string
                                       :description "Paragraph ID such as 2.5, as shown in the eww_read output.")
                                  :quote (:type string
                                          :description "Short excerpt copied verbatim from that paragraph, no paraphrase."))))))))

(defconst gptel-eww-directive
  "When a question needs current or external information, use eww_search, then eww_read on the best pages.  Before answering, call eww_cite once with the paragraph ID and a short verbatim quote for every fact you will use.  Then answer, putting the paragraph ID after each claim taken from a page, e.g. [2.5].  Do not cite anything you did not read, and do not invent IDs."
  "Optional text to add to your gptel system prompt for more reliable citing.")

;;;###autoload
(defun gptel-eww-register ()
  "Add the eww tools to `gptel-tools' and enable automatic citation handling."
  (interactive)
  (dolist (tool gptel-eww-tools)
    (setq gptel-tools
          (cons tool (cl-remove (gptel-tool-name tool) gptel-tools
                                :key #'gptel-tool-name :test #'equal))))
  (add-hook 'gptel-post-response-functions #'gptel-eww--process-response)
  (message "gptel-eww: tools registered, citations will be highlighted"))

;;;; Commands

;;;###autoload
(defun gptel-eww-show-sources ()
  "Jump to one of the pages read so far."
  (interactive)
  (let* ((live (cl-remove-if-not
                (lambda (s) (buffer-live-p (gptel-eww-source-buffer s)))
                gptel-eww--sources))
         (alist (mapcar (lambda (s)
                          (cons (format "[%d] %s — %s"
                                        (gptel-eww-source-n s)
                                        (or (gptel-eww-source-title s) "")
                                        (gptel-eww-source-url s))
                                s))
                        live)))
    (if (null alist)
        (user-error "No sources read yet")
      (let* ((s (cdr (assoc (completing-read "Source: " alist nil t) alist)))
             (buf (gptel-eww-source-buffer s))
             (ov (with-current-buffer buf
                   (cl-find-if (lambda (o) (overlay-get o 'gptel-eww-cite))
                               (overlays-in (point-min) (point-max))))))
        (gptel-eww--goto buf (if ov (overlay-start ov) 1))))))

;;;###autoload
(defun gptel-eww-clear-highlights ()
  "Remove citation highlights from all source buffers."
  (interactive)
  (clrhash gptel-eww--cited)
  (dolist (s gptel-eww--sources)
    (let ((buf (gptel-eww-source-buffer s)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (remove-overlays (point-min) (point-max) 'gptel-eww-cite t))))))

;;;###autoload
(defun gptel-eww-kill-sources ()
  "Kill all source buffers and forget the sources."
  (interactive)
  (dolist (s gptel-eww--sources)
    (when (buffer-live-p (gptel-eww-source-buffer s))
      (kill-buffer (gptel-eww-source-buffer s))))
  (clrhash gptel-eww--cited)
  (setq gptel-eww--sources nil))

(provide 'gptel-eww-search)
;;; gptel-eww-search.el ends here
