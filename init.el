;; -*- lexical-binding: t; -*-

(require 'mortal-base-config)

(setq mortal-move-style 'smart)

(use-package mortal-mode
  :load-path "~/.emacs.d/mortal-mode"
  :config
  (mortal-mode 1))

