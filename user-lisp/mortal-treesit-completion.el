;;; -*- lexical-binding: t; -*-

;;; mortal-treesit-completion.el --- Tree-sitter completion for Mortal

(require 'treesit)
(require 'completion-preview)


;;; ---------------------------------------------------------------------------
;;; Tree-sitter completion
;;; ---------------------------------------------------------------------------

(defun mortal/treesit-completion ()
  "Complete symbols found in the current Tree-sitter buffer."
  (when (treesit-parser-list)
    (let ((beg (save-excursion
                 (skip-syntax-backward "w_")
                 (point)))
          (end (point)))
      (list
       beg
       end
       (completion-table-dynamic
        (lambda (_)
          (let (symbols)
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward
                      "\\_<[[:word:]_]+\\_>"
                      nil
                      t)
                (push (match-string-no-properties 0)
                      symbols)))
            (delete-dups symbols))))))))


(defun mortal/treesit-completion-setup ()
  "Enable Tree-sitter completion in the current buffer."
  (when (treesit-parser-list)
    (add-hook 'completion-at-point-functions
              #'mortal/treesit-completion
              nil
              t)
    (completion-preview-mode 1)))


(add-hook 'prog-mode-hook #'mortal/treesit-completion-setup)


;;; ---------------------------------------------------------------------------
;;; End
;;; ---------------------------------------------------------------------------

(provide 'mortal-treesit-completion)
;;; mortal-treesit-completion.el ends here
