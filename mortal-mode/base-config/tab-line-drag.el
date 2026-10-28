;;; tab-line-drag.el --- Move tabs between windows -*- lexical-binding: t; -*-

(require 'seq)
(require 'tab-line)

(defun my/tab-line--tab-buffer (string)
  "Return the buffer of the tab under propertized STRING, or nil."
  (let ((tab (and string (tab-line--get-tab-property 'tab string))))
    (if (bufferp tab) tab (cdr (assq 'buffer tab)))))

(defun my/tab-line--reorder (event window buffer target-buffer)
  "Handle a drop inside the same WINDOW: reorder BUFFER around TARGET-BUFFER."
  (cond
   ((eq tab-line-tabs-function #'tab-line-tabs-fixed-window-buffers)
    (when (and target-buffer (not (eq target-buffer buffer)))
      (let* ((tabs (with-selected-window window
                     (funcall tab-line-tabs-function)))
             (from (seq-position tabs buffer))
             (to (seq-position tabs target-buffer)))
        (when (and from to)
          (let* ((others (remq buffer (copy-sequence tabs)))
                 (pos (seq-position others target-buffer))
                 ;; Dragging right lands after the target, left before it.
                 (pos (if (< from to) (1+ pos) pos)))
            (set-window-parameter
             window 'tab-line-buffers
             (append (seq-take others pos)
                     (list buffer)
                     (seq-drop others pos)))
            (tab-line-force-update t))))))
   ;; Other providers: fall back to Emacs' own reordering if it exists.
   ((fboundp 'tab-line-mouse-move-tab)
    (tab-line-mouse-move-tab event))))

(defun my/tab-line-move-tab (event)
  "Move the dragged tab to another window, or reorder it in its own."
  (interactive "e")
  (let* ((start (tab-line-event-start event))
         (end (event-end event))
         (source (posn-window start))
         (target (posn-window end))
         (buffer (my/tab-line--tab-buffer (car (posn-string start))))
         (target-buffer (my/tab-line--tab-buffer (car (posn-string end)))))
    (when (and (buffer-live-p buffer)
               (window-live-p source)
               (window-live-p target)
               (not (window-minibuffer-p target)))
      (if (eq source target)
          (my/tab-line--reorder event source buffer target-buffer)
        (unless (memq tab-line-tabs-function
                      '(tab-line-tabs-fixed-window-buffers
                        tab-line-tabs-window-buffers))
          (user-error "Tab moving requires window-local tab lists"))
        (let* ((source-tabs
                (with-selected-window source
                  (funcall tab-line-tabs-function)))
               (target-tabs
                (with-selected-window target
                  (funcall tab-line-tabs-function)))
               (fixed-order-p
                (eq tab-line-tabs-function
                    #'tab-line-tabs-fixed-window-buffers))
               (source-buffers (remq buffer (copy-sequence source-tabs)))
               ;; Moving the last tab closes the source window.
               (close-source-p
                (and (null source-buffers)
                     (eq (window-deletable-p source) t)))
               (replacement
                (or (seq-find #'buffer-live-p source-buffers)
                    (other-buffer buffer))))
          ;; Show the dragged buffer in the destination window.
          (set-window-buffer target buffer)
          (unless close-source-p
            (when (eq (window-buffer source) buffer)
              (set-window-buffer source replacement)))

          ;; Remove the buffer from the source window's navigation history.
          (set-window-prev-buffers
           source
           (seq-remove (lambda (entry) (eq (car entry) buffer))
                       (window-prev-buffers source)))
          (set-window-next-buffers
           source
           (remq buffer (copy-sequence (window-next-buffers source))))

          ;; Maintain the explicit tab order of the fixed-window provider.
          ;; Insert before the tab under the pointer, else append.
          (when fixed-order-p
            (let* ((target-buffers
                    (remq buffer (copy-sequence target-tabs)))
                   (position
                    (and target-buffer
                         (seq-position target-buffers target-buffer))))
              (set-window-parameter source 'tab-line-buffers source-buffers)
              (set-window-parameter
               target 'tab-line-buffers
               (if position
                   (append (seq-take target-buffers position)
                           (list buffer)
                           (seq-drop target-buffers position))
                 (append target-buffers (list buffer))))))

          (when close-source-p
            (delete-window source))
          (select-window target)

          ;; Invalidate cached tab lines in all windows.
          (tab-line-force-update t))))))

;; Drag a tab onto another window to move its buffer there.
(define-key tab-line-tab-map [tab-line drag-mouse-1] #'my/tab-line-move-tab)

(provide 'tab-line-drag)
;;; tab-line-drag.el ends here
