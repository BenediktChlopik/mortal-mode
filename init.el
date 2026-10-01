;; -*- lexical-binding: t; -*-

(require 'mortal-base-config)


(setq mortal-move-style 'smart)

(require 'mortal-mode)

(mortal-mode 1)


(add-to-list 'load-path
             (expand-file-name "your-config-files/"
                               user-lisp-directory))

(require 'alternative-keybinds)
(require 'your-config-init)

