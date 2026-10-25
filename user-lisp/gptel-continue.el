;;; gptel-continue.el --- Retry a failed gptel request, reusing context and tool calls -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; `gptel-continue' retries the last gptel request in the current buffer
;; if it failed (HTTP error, network error, abort, timeout...).
;;
;; Instead of rebuilding the prompt from the buffer, it re-sends the exact
;; payload (the :data entry of the failed request's FSM info).  That payload
;; already contains the conversation context AND every tool call and tool
;; result gathered so far in the multi-turn tool loop, so:
;;
;;   - tools that already ran are NOT executed again,
;;   - the model sees the same context it saw before the failure,
;;   - the request goes through the normal gptel FSM (streaming, callbacks,
;;     tool handling, post-response hooks all behave as usual).
;;
;; If no failed request is remembered (e.g. the buffer was restored from
;; disk), it falls back to `gptel-send', which rebuilds the prompt from the
;; buffer, including tool calls recorded there.
;;
;; Usage:
;;   (require 'gptel-continue)
;;   (define-key gptel-mode-map (kbd "C-c C-r") #'gptel-continue)
;;   M-x gptel-continue

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'gptel-request)

(defgroup gptel-continue nil
  "Retry failed gptel requests while reusing context and tool calls."
  :group 'gptel)

(defcustom gptel-continue-fallback-to-send t
  "If non-nil, `gptel-continue' calls `gptel-send' when no failed request exists.
When nil, it signals a `user-error' instead."
  :type 'boolean)

(defcustom gptel-continue-max-retries nil
  "Maximum number of consecutive retries of the same request.
nil means unlimited."
  :type '(choice (const :tag "Unlimited" nil) integer))

(defconst gptel-continue--transient-keys
  '(:error :status :http-status :tool-use :tool-success :tracking-marker
    :partial_text :stop-reason)
  "Keys cleared from the FSM info before re-sending a request.
They describe the outcome of the failed attempt, not the request itself.
`:data' (context + tool calls/results) is deliberately preserved.")

(defun gptel-continue--failed-p (fsm)
  "Return non-nil if FSM represents a failed or aborted request."
  (and fsm
       (gptel-fsm-p fsm)
       (let ((info (gptel-fsm-info fsm)))
         (and (plist-get info :data)
              (or (plist-get info :error)
                  (memq (gptel-fsm-state fsm) '(ERRS ABRT)))))))

(defun gptel-continue--clean-info (info)
  "Return a copy of INFO without outcome-related keys, with retry count bumped."
  (let (clean)
    (cl-loop for (k v) on info by #'cddr
             unless (memq k gptel-continue--transient-keys)
             do (setq clean (nconc clean (list k v))))
    (plist-put clean :retry-count (1+ (or (plist-get info :retry-count) 0)))))

(defun gptel-continue--retry (fsm)
  "Re-send the request of the failed FSM, reusing its payload."
  (let* ((old-info (gptel-fsm-info fsm))
         (retries (or (plist-get old-info :retry-count) 0)))
    (when (and gptel-continue-max-retries
               (>= retries gptel-continue-max-retries))
      (user-error "gptel-continue: reached maximum of %d retries"
                  gptel-continue-max-retries))
    (let* ((info (gptel-continue--clean-info old-info))
           (buf (or (plist-get info :buffer) (current-buffer)))
           (new (gptel-make-fsm :table (gptel-fsm-table fsm)
                                :handlers (gptel-fsm-handlers fsm)
                                :info info)))
      (unless (buffer-live-p buf)
        (user-error "gptel-continue: original buffer no longer exists"))
      (with-current-buffer buf
        (setq-local gptel--fsm-last new)
        (when (fboundp 'gptel--update-status)
          (gptel--update-status " Retrying..." 'warning))
        (message "gptel-continue: retrying (attempt %d)"
                 (plist-get info :retry-count))
        ;; INIT -> WAIT: the WAIT handler sends (:data info) as-is.
        (gptel--fsm-transition new))
      new)))

;;;###autoload
(defun gptel-continue ()
  "Retry the last failed gptel request in this buffer.
The previous context and all tool calls/results already collected are
re-sent unchanged, so completed tool calls are not run again.

If there is no failed request to retry, fall back to `gptel-send'
(see `gptel-continue-fallback-to-send')."
  (interactive)
  (let ((fsm (bound-and-true-p gptel--fsm-last)))
    (cond
     ((gptel-continue--failed-p fsm)
      (gptel-continue--retry fsm))
     (gptel-continue-fallback-to-send
      (message "gptel-continue: no failed request found, using `gptel-send'")
      (call-interactively #'gptel-send))
     (t (user-error "gptel-continue: no failed request to retry")))))

(provide 'gptel-continue)
;;; gptel-continue.el ends here
