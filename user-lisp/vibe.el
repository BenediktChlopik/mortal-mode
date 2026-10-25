;;; -*- lexical-binding: t; -*-

;;; ===========================================================================
;;; Package Management
;;; ===========================================================================

(require 'package)
(declare-function gptel-make-openai "gptel-openai")

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)

(require 'use-package)


;;; ===========================================================================
;;; Terminal Emulator: vterm
;;; Command: M-x vterm
;;; ===========================================================================

(use-package vterm
  :commands (vterm))

(add-to-list
 'display-buffer-alist
 '("\\`\\*vterm"
   (display-buffer-in-side-window)
   (side . bottom)
   (slot . 0)
   (window-height . 0.3)))


;;; ===========================================================================
;;; AI Chat: gptel
;;; Command: M-x gptel
;;; ===========================================================================

(declare-function gptel-make-openai "gptel-openai")
(declare-function gptel-get-tool "gptel" (path))
(declare-function pyscn-gptel-register "pyscn-gptel-register")


(use-package gptel
  :demand t
  :config
  (require 'gptel-integrations)

  (require 'gptel-continue)
  (require 'pyscn-gptel)
  (pyscn-gptel-register)
  
  (setq gptel-include-reasoning nil
        gptel-display-buffer-action
        '((display-buffer-in-side-window)
          (side . right)
          (slot . 0)
          (window-width . 0.40)))

  ;; Global default tools for all gptel requests.
  (setq gptel-tools
        (delq nil
              (mapcar #'gptel-get-tool
                      (mapcar #'gptel-get-tool
                              '(("web" "eww_search")
                                ("web" "eww_read")
                                ("web" "eww_cite")
                                ("flymake" "flymake_list_buffers")
                                ("flymake" "flymake_diagnostics")
                                ("flymake" "flymake_read_lines")
                                ("flymake" "flymake_recheck")
                                ("vterm" "vterm_type")
                                ("pyscn" "pyscn_analyze_code")
                                ("pyscn" "pyscn_check_complexity")
                                ("pyscn" "pyscn_detect_clones")
                                ("pyscn" "pyscn_check_coupling")
                                ("pyscn" "pyscn_find_dead_code")
                                ("pyscn" "pyscn_get_health_score"))))))

  (setq gptel-backend
        (gptel-make-openai "LiteLLM"
          :host "127.0.0.1:4000"
          :protocol "http"
          :endpoint "/v1/chat/completions"
          :stream t
          :key "6ef55eaf135d16ca4f88abc23d303723bdc15b29b4b362498c73fb64695d08a6"
          :models '(free))
        gptel-model 'free))


;;; ===========================================================================
;;; AI-Assisted Editing: Macher
;;; ===========================================================================

  (defun vibe/macher-action-buffer-setup ()
    (setq-local window-point-insertion-type t)
    (visual-line-mode 1))

  (use-package macher
    :after gptel
    :custom
    (macher-action-buffer-ui 'org)
    :hook
    (macher-action-buffer-setup . vibe/macher-action-buffer-setup)
    :config
    (macher-install)
    (macher-enable)
    (add-to-list
     'display-buffer-alist
     '("\\*macher-patch:.*\\*"
       (display-buffer-in-side-window)
       (side . right))))


;;; ===========================================================================
;;; Graph of Thoughts: got
;;; Commands: M-x got-start, M-x got-stop, M-x got-tools, M-x got-ask
;;; ===========================================================================

  (use-package got
    :ensure nil
    :after (gptel mcp)
    :load-path "/usr/bin/emacs"
    :commands (got-start got-stop got-tools got-ask))


;;; ===========================================================================
;;; Graph Visualization: graph-fa2
;;; ===========================================================================

  (use-package graph-fa2
    :vc (:url "https://github.com/elij/graph-fa2"))


;;; ===========================================================================
;;; extra keys
;;; ===========================================================================

(require 'mortal-keymap)
(mortal/add-keys
 "C-t" #'vterm
 "C-f" #'deadgrep)


;;; ===========================================================================
;;; End of Configuration
;;; ===========================================================================
