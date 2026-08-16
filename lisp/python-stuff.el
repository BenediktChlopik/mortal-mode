(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-auto-prefix 2))

(use-package eglot
  :hook (python-base-mode . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((python-base-mode :language-id "python") . ("ty" "server")))
  (add-to-list 'eglot-ignored-server-capabilities :inlayHintProvider))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'eglot-completion-at-point))))

(require 'pyvenv)
(pyvenv-mode 1)
