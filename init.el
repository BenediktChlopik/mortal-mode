;; -*- lexical-binding: t; -*-

(use-package mortal-mode
  :load-path "~/.emacs.d/mortal-mode"
  :config
  (mortal-mode 1))

(setq mortal-move-style 'smart)


(add-to-list 'load-path "~/.emacs.d/mortal-mode/base-config")

(require 'mortal-base-config)



;; Add keys like this:
;; (require 'mortal-keymap)
;; (mortal/add-keys
;;  "C-=" #'foo
;;  "M-/" #'bar
;;  "<f5>" my-prefix-map)
