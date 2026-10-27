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

(declare-function vterm--self-insert "vterm")
(declare-function vterm-yank "vterm")

(use-package vterm
  :commands (vterm)
  :init
  (setq vterm-keymap-exceptions '("C-c"))
  :config
  (define-key vterm-mode-map (kbd "C-c") #'vterm--self-insert)
  (define-key vterm-mode-map (kbd "C-v") #'vterm-yank)
  (define-key vterm-mode-map (kbd "C-S-v") #'vterm-yank))

(add-to-list
 'display-buffer-alist
 '("\\`\\*vterm"
   (display-buffer-in-side-window)
   (side . bottom)
   (slot . 0)
   (window-height . 0.3)))




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

(use-package macher-agent
  :vc (:url "https://github.com/elij/macher-agent/")
  :ensure t
  :after (gptel macher)
  :hook (gptel-mode . macher-agent-mode)
  :config
  (require 'macher-agent-vfs)      ;; Enable Virtual File System
  (require 'macher-agent-sandbox)  ;; Enable Programmatic Tool Calling
  (require 'macher-agent-zero-mem) ;; Enable dual-ledger conversation memory
  (macher-agent-install))

;;; ===========================================================================
;;; AI Chat: gptel
;;; Command: M-x gptel
;;; ===========================================================================

(declare-function gptel-make-openai "gptel-openai")
(declare-function gptel-get-tool "gptel" (path))
(declare-function pyscn-gptel-register "pyscn-gptel-register")
(declare-function gptel-make-tool "gptel")
(declare-function macher-agent-install "macher-agent")

(use-package gptel
  :demand t
  :config
  (require 'gptel-integrations)
  (require 'macher)
  (require 'gptel-continue)
  (require 'pyscn-gptel)
  (require 'markdown-mode)
  (pyscn-gptel-register)
  
  (setq gptel-include-reasoning nil
        gptel-display-buffer-action
        '((display-buffer-in-side-window)
          (side . right)
          (slot . 0)
          (window-width . 0.40))
        gptel-default-mode 'markdown-mode)

  
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
                                ("vterm" "vterm_read")
                                ("macher" "search_in_workspace")
                                ("macher" "list_directory_in_workspace")
                                ("macher" "read_file_in_workspace")
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

(defun vibe/gptel-send-buffer-or-region ()
  "Send the active region, or the entire buffer if no region is active."
  (interactive)
  (unless (use-region-p)
    (goto-char (point-max)))
  (gptel-send))

(defun vibe/gptel-python-check (file)
  "Compile-check a Python FILE inside the current project directory."
  (let* ((root (file-name-as-directory
                (file-truename default-directory)))
         (path (expand-file-name file root)))
    (unless (and (file-regular-p path)
                 (string-equal (file-name-extension path) "py")
                 (file-in-directory-p path root))
      (error "Expected an existing .py file inside the project root"))
    (with-temp-buffer
      (let ((status
             (call-process "python3" nil t nil
                           "-m" "py_compile" path)))
        (if (and (integerp status) (zerop status))
            (format "Python syntax check PASSED: %s" file)
          (format "Python syntax check FAILED (exit %s):\n%s"
                  status (buffer-string)))))))


(add-to-list
 'gptel-tools
 (gptel-make-tool
  :name "python_check"
  :function #'vibe/gptel-python-check
  :description
  "Compile-check an existing Python file for syntax errors.
   Pass a project-relative path such as src/main.py.
   Returns success or Python compiler diagnostics.
   Does not execute the Python program."
  :args
  (list
   '(:name "file"
     :type string
     :description "Project-relative path to the Python file"))
  :category "python"))

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
  :load-path "~/projects/graph-fa2"
  :custom
  (graph-fa2-engine '3d)
  (graph-fa2-framerate 60.0))

(setq got-server-script
      "/home/raptus/projects/Adaptive-Graph-of-Thoughts-MCP-server/launch.sh")


;;; ===========================================================================
;;; extra keys
;;; ===========================================================================

(require 'deadgrep)
(require 'mortal-keymap)
(mortal/add-keys
 "C-t" #'vterm
 "C-f" #'deadgrep
 "<f5>" #'vibe/gptel-send-buffer-or-region)


;;; ===========================================================================
;;; End of Configuration
;;; ===========================================================================
 
