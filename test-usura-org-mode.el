;;; test-usura-org-mode.el --- Test the org-mode UI for usura -*- lexical-binding: t; -*-

;;; Commentary:
;; Test file to demonstrate the org-mode based Claude UI

;;; Code:

(require 'usura-mode)

;; Test the org-mode UI
(defun test-usura-org-basic ()
  "Test basic org-mode output."
  (interactive)
  ;; Ensure org-mode UI is enabled
  (setq usura-use-org-mode t)
  (usura-claude-stream "Write a haiku about Emacs"))

(defun test-usura-org-with-code ()
  "Test org-mode output with code examples."
  (interactive)
  (setq usura-use-org-mode t)
  (usura-claude-stream "Show me a simple elisp function that reverses a string"))

(defun test-usura-org-with-tools ()
  "Test org-mode output with tool usage."
  (interactive)
  (setq usura-use-org-mode t)
  (usura-claude-stream "/file list the current directory"))

(defun test-usura-org-advice ()
  "Test org-mode advice on current buffer."
  (interactive)
  (setq usura-use-org-mode t)
  (usura-claude-advice))

(defun test-usura-toggle-ui ()
  "Toggle between org-mode and regular UI."
  (interactive)
  (setq usura-use-org-mode (not usura-use-org-mode))
  (message "Usura org-mode UI %s" 
           (if usura-use-org-mode "enabled" "disabled")))

;; Demo function to show all features
(defun test-usura-org-demo ()
  "Run a comprehensive demo of the org-mode UI."
  (interactive)
  (setq usura-use-org-mode t)
  (usura-claude-stream 
   "Create a simple Python function to calculate fibonacci numbers. 
    Include:
    1. The function implementation
    2. Example usage
    3. Time complexity analysis
    Please think through the problem first."))

(provide 'test-usura-org-mode)
;;; test-usura-org-mode.el ends here