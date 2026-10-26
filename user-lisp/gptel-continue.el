;;; gptel-continue.el --- Retry a failed gptel request, reusing context and tool calls -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (gptel "0.9.8"))

;;; Commentary:

;; `gptel-continue' retries the last gptel request in the current buffer
;; if it failed (HTTP error, network error, curl failure, abort...).
;;
;; Instead of rebuilding the prompt from the buffer, it re-sends the exact
;; payload of the failed request.  That payload already contains the
;; conversation context AND every tool call and tool result gathered so far
;; in the multi-turn tool loop, so tools that already ran are NOT run again.
;;
;; Two facts about gptel's internals shape this package:
;;
;; 1. On an error, gptel erases the conversation from the request data
;;    (:messages, :contents, :query) unless `gptel-log-level' is set.  We
;;    therefore snapshot the payload just before that happens (advice on
;;    `gptel--fsm-last').
;;
;; 2. A failed streaming request may have left a partial response in the
;;    buffer.  We remember where each attempt started inserting (advice on
;;    `gptel--handle-wait') and delete that partial text before retrying, so
;;    the new response does not get appended to the old fragment.
;;
;; If the payload is unavailable (e.g. the request failed before this file
;; was loaded, or the buffer was restored from disk), it falls back to
;; `gptel-send', which rebuilds the prompt from the buffer.
;;
;; Usage:
;;   (require 'gptel-continue)
;;   (define-key gptel-mode-map (kbd "C-c C-r") #'gptel-continue)
;;   M-x gptel-continue      ;retry the failed request
;;   C-u M-x gptel-continue  ;force a rebuild from the buffer via `gptel-send'

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gptel)
(require 'gptel-request)

(declare-function gptel-send "gptel")
(declare-function gptel--fsm-transition "gptel-request")

(defgroup gptel-continue nil
  "Retry failed gptel requests while reusing context and tool calls."
  :group 'gptel)

(defcustom gptel-continue-fallback-to-send t
  "If non-nil, `gptel-continue' may fall back to `gptel-send'.
This applies when no usable request payload is available.  When nil,
it signals a `user-error' instead."
  :type 'boolean)

(defcustom gptel-continue-max-retries nil
  "Maximum number of consecutive retries of the same request.
nil means unlimited."
  :type '(choice (const :tag "Unlimited" nil) natnum))

(defcustom gptel-continue-discard-partial t
  "If non-nil, delete the partial response of the failed attempt before retrying.
Only text inserted by the failed attempt is removed; earlier output of
the same request (such as completed tool calls) is kept."
  :type 'boolean)

(defconst gptel-continue--transient-keys
  '(:tool-use :tool-result :tool-pending :error :status :http-status
    :stop-reason :reasoning :reasoning-block :reasoning-marker
    :reasoning-chunks :partial_json :partial_text :partial_reasoning
    :message-complete :accumulated-events :signature :citations :sources
    :tokens :end-time :continue-data :continue-origin)
  "Keys of the request info plist that describe one attempt's outcome.
They are dropped when building the info for a retry.  `:data' is rebuilt
from the snapshot, and everything else (callback, backend, buffer,
markers, tools...) is kept.")

(defconst gptel-continue--data-keys '(:messages :contents :query :input)
  "Keys of the request data that hold the conversation.")

;;;; Payload snapshot and attempt origin

(defun gptel-continue--data-usable-p (data)
  "Return non-nil if request DATA still holds a conversation.
gptel blanks :messages, :contents and :query after an error, so data
where every conversation key present is nil is not usable."
  (and (consp data)
       (let ((present (cl-remove-if-not (lambda (k) (plist-member data k))
                                        gptel-continue--data-keys)))
         (or (null present)             ;Unknown backend layout: trust it
             (cl-some (lambda (k) (plist-get data k)) present)))))

(defun gptel-continue--snapshot (fsm)
  "Save a copy of FSM's request payload if FSM failed.
Meant as :before advice on `gptel--fsm-last', which erases the data."
  (when (and (gptel-fsm-p fsm) (eq (gptel-fsm-state fsm) 'ERRS))
    (condition-case err
        (let* ((info (gptel-fsm-info fsm))
               (data (plist-get info :data)))
          (when (gptel-continue--data-usable-p data)
            (plist-put info :continue-data (copy-tree data t))))
      (error (message "gptel-continue: could not snapshot request: %S" err)))))

(defun gptel-continue--record-origin (fsm)
  "Record where the attempt about to be sent by FSM will start inserting.
Meant as :before advice on `gptel--handle-wait'."
  (condition-case nil
      (let* ((info (gptel-fsm-info fsm))
             (tm (plist-get info :tracking-marker))
             (pos (plist-get info :position))
             (base (if (markerp tm) tm pos)))
        (when (and (markerp base) (marker-buffer base))
          (plist-put info :continue-origin
                     (cons (copy-marker base nil) (and (markerp tm) t)))))
    (error nil)))

(defun gptel-continue--discard-partial (info)
  "Delete text inserted by the failed attempt described by INFO.
Reset INFO's markers so that the retry starts inserting where that
attempt did.  Never signals an error."
  (condition-case err
      (when-let* ((origin-rec (plist-get info :continue-origin))
                  (origin (car origin-rec))
                  ((markerp origin))
                  ((marker-buffer origin))
                  ((not (plist-get info :in-place)))
                  (tm (plist-get info :tracking-marker))
                  ((markerp tm))
                  ((eq (marker-buffer tm) (marker-buffer origin))))
        (with-current-buffer (marker-buffer origin)
          (when (> tm origin)
            (let ((inhibit-read-only t))
              (delete-region origin tm))))
        (when-let* ((pos (plist-get info :position))
                    ((markerp pos))
                    ((eq (marker-buffer pos) (marker-buffer origin)))
                    ((not (cdr origin-rec)))) ;No tracking marker at attempt start
          ;; The attempt created the tracking marker and moved the start
          ;; marker past the separator/prefix: undo both.
          (move-marker pos origin)
          (plist-put info :tracking-marker nil)))
    (error (message "gptel-continue: could not discard partial response: %S"
                    err))))

;;;; Retrying

(defun gptel-continue--failed-p (fsm)
  "Return non-nil if FSM is a failed or aborted request."
  (and (gptel-fsm-p fsm) (memq (gptel-fsm-state fsm) '(ERRS ABRT)) t))

(defun gptel-continue--live-p (fsm)
  "Return non-nil if FSM is an ongoing request."
  (and (gptel-fsm-p fsm)
       (not (memq (gptel-fsm-state fsm) '(ERRS ABRT DONE)))))

(defun gptel-continue--payload (fsm)
  "Return the best saved request payload of FSM, or nil if none is usable."
  (let ((info (gptel-fsm-info fsm)))
    (cl-find-if #'gptel-continue--data-usable-p
                (list (plist-get info :continue-data)
                      (plist-get info :data)))))

(defun gptel-continue--build-info (info payload)
  "Return a fresh info plist for a retry of INFO using PAYLOAD."
  (let (new)
    (cl-loop for (k v) on info by #'cddr
             unless (memq k gptel-continue--transient-keys)
             do (push k new) (push v new))
    (setq new (nreverse new))
    (setq new (plist-put new :data (copy-tree payload t)))
    (plist-put new :retry-count (1+ (or (plist-get info :retry-count) 0)))))

(defun gptel-continue--check-retry-allowed (info)
  "Signal a `user-error' if INFO's request may not be retried."
  (let ((buf (plist-get info :buffer)))
    (unless (buffer-live-p buf)
      (user-error "gptel-continue: the request's buffer no longer exists"))
    (when (and gptel-continue-max-retries
               (>= (or (plist-get info :retry-count) 0)
                   gptel-continue-max-retries))
      (user-error "gptel-continue: reached the maximum of %d retries"
                  gptel-continue-max-retries))))

(defun gptel-continue--retry (fsm payload)
  "Re-send the request of failed FSM using PAYLOAD.  Return the new FSM."
  (let ((info (gptel-fsm-info fsm)))
    (gptel-continue--check-retry-allowed info)
    (when gptel-continue-discard-partial
      (gptel-continue--discard-partial info))
    (let* ((new-info (gptel-continue--build-info info payload))
           (new (gptel-make-fsm :table (gptel-fsm-table fsm)
                                :handlers (gptel-fsm-handlers fsm)
                                :info new-info)))
      (with-current-buffer (plist-get info :buffer)
        (message "gptel-continue: retrying (attempt %d)"
                 (plist-get new-info :retry-count))
        ;; INIT -> WAIT: the WAIT handler fires the request and the
        ;; gptel-send handlers record NEW as `gptel--fsm-last'.
        (gptel--fsm-transition new))
      new)))

(defun gptel-continue--fallback (fsm)
  "Rebuild the prompt from the buffer and send it with `gptel-send'.
If FSM is a failed request, first discard its partial response and move
point to where it was sent from."
  (unless gptel-continue-fallback-to-send
    (user-error "gptel-continue: no usable request data to retry"))
  (when (gptel-continue--failed-p fsm)
    (let* ((info (gptel-fsm-info fsm))
           (buf (plist-get info :buffer))
           (pos (plist-get info :position)))
      (when gptel-continue-discard-partial
        (gptel-continue--discard-partial info))
      (when (and (buffer-live-p buf) (markerp pos)
                 (eq (marker-buffer pos) buf)
                 (eq buf (current-buffer)))
        (goto-char pos))))
  (gptel-send))

;;;###autoload
(defun gptel-continue (&optional rebuild)
  "Retry the last failed gptel request in this buffer.
The previous context and all tool calls and results already collected
are re-sent unchanged, so completed tool calls are not run again.

If the request data is unavailable, rebuild the prompt from the buffer
with `gptel-send' instead (see `gptel-continue-fallback-to-send').

With prefix argument REBUILD, always rebuild the prompt from the buffer
with `gptel-send', even if the last request did not fail."
  (interactive "P")
  (let ((fsm (and (boundp 'gptel--fsm-last) gptel--fsm-last)))
    (cond
     ((gptel-continue--live-p fsm)
      (user-error "gptel-continue: a request is still in progress in this buffer"))
     (rebuild
      (gptel-continue--fallback fsm))
     ((not (gptel-fsm-p fsm))
      (unless gptel-continue-fallback-to-send
        (user-error "gptel-continue: no previous request in this buffer"))
      (when (or (not (called-interactively-p 'any))
                (y-or-n-p "No failed gptel request recorded.  Re-send this buffer with `gptel-send'? "))
        (gptel-continue--fallback nil)))
     ((eq (gptel-fsm-state fsm) 'DONE)
      (user-error "gptel-continue: the last request succeeded; nothing to continue (C-u to re-send)"))
     ((gptel-continue--failed-p fsm)
      (if-let* ((payload (gptel-continue--payload fsm)))
          (gptel-continue--retry fsm payload)
        (message "gptel-continue: request data unavailable, rebuilding from buffer")
        (gptel-continue--fallback fsm)))
     (t (user-error "gptel-continue: unexpected request state %S"
                    (gptel-fsm-state fsm))))))

;;;; Installation

(advice-add 'gptel--fsm-last :before #'gptel-continue--snapshot)
(advice-add 'gptel--handle-wait :before #'gptel-continue--record-origin)

(defun gptel-continue-unload-function ()
  "Remove the advice installed by `gptel-continue'."
  (advice-remove 'gptel--fsm-last #'gptel-continue--snapshot)
  (advice-remove 'gptel--handle-wait #'gptel-continue--record-origin)
  nil)

(provide 'gptel-continue)
;;; gptel-continue.el ends here
