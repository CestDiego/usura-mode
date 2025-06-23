;;; test-claude-commands.el --- Test dynamic Claude commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Test the dynamic command generation system

;;; Code:

(require 'usura-mode)
(require 'usura-claude-commands)

(defun test-claude-commands-basic ()
  "Test basic command scanning."
  (interactive)
  (message "Commands directory: %s" usura-claude-commands-dir)
  (message "Scanning for commands...")
  (usura-claude-commands-scan)
  (message "Found %d commands" (length usura-claude-commands-alist))
  (dolist (cmd usura-claude-commands-alist)
    (message "  - /%s from %s" (car cmd) (file-name-nondirectory (cdr cmd)))))

(defun test-claude-commands-functions ()
  "Test that functions were created."
  (interactive)
  (usura-claude-commands-scan)
  (dolist (cmd usura-claude-commands-alist)
    (let ((func-name (usura-claude-commands--make-function-name (car cmd))))
      (if (fboundp func-name)
          (message "✓ Function %s exists" func-name)
        (message "✗ Function %s NOT FOUND" func-name)))))

(defun test-claude-commands-keybindings ()
  "Test keybinding assignments."
  (interactive)
  (usura-claude-commands-scan)
  (let ((index 0))
    (dolist (cmd usura-claude-commands-alist)
      (let* ((command-name (car cmd))
             (func-name (usura-claude-commands--make-function-name command-name))
             (key (usura-claude-commands--make-keybinding command-name index)))
        (message "Command /%s -> %s -> %s" 
                 command-name 
                 func-name
                 (key-description key)))
      (cl-incf index))))

(defun test-claude-commands-create-sample ()
  "Create sample command files for testing."
  (interactive)
  (let ((commands-dir (expand-file-name usura-claude-commands-dir)))
    (unless (file-directory-p commands-dir)
      (make-directory commands-dir t))
    
    ;; Create sample commands
    (with-temp-file (expand-file-name "test-explain.md" commands-dir)
      (insert "# Explain Command\n\nExplain the selected code or text.\n"))
    
    (with-temp-file (expand-file-name "test-review.md" commands-dir)
      (insert "# Code Review Command\n\nReview code for best practices.\n"))
    
    (with-temp-file (expand-file-name "test-summarize.md" commands-dir)
      (insert "# Summarize Command\n\nCreate a summary of the content.\n"))
    
    (message "Created sample commands in %s" commands-dir)
    (usura-claude-commands-scan)))

(defun test-claude-commands-advice ()
  "Test the advice command specifically."
  (interactive)
  (if (fboundp 'usura-claude-advice)
      (message "✓ usura-claude-advice function exists")
    (message "✗ usura-claude-advice NOT FOUND"))
  
  ;; Check if dynamic version would work
  (let ((advice-file (expand-file-name "advice.md" usura-claude-commands-dir)))
    (if (file-exists-p advice-file)
        (message "✓ advice.md found at %s" advice-file)
      (message "✗ advice.md NOT FOUND - expected at %s" advice-file))))

(defun test-claude-commands-all ()
  "Run all tests."
  (interactive)
  (test-claude-commands-basic)
  (message "---")
  (test-claude-commands-functions)
  (message "---")
  (test-claude-commands-keybindings)
  (message "---")
  (test-claude-commands-advice))

(provide 'test-claude-commands)
;;; test-claude-commands.el ends here