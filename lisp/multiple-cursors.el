(use-package multiple-cursors
  :ensure t
  :bind (("C-M-<down>" . mc/mark-next-like-this)
         ("C-M-<up>" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

