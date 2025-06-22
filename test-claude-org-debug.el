;;; test-claude-org-debug.el --- Debug org-mode UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Debug functions for org-mode UI

;;; Code:

(require 'usura-mode)
(require 'claude-stream-org-ui)

(defun test-claude-org-simple ()
  "Test with a simple prompt."
  (interactive)
  (setq usura-use-org-mode t)
  ;; Use a simple test command
  (let ((prompt "Say hello"))
    (message "Testing with prompt: %s" prompt)
    (usura-claude-stream prompt)))

(defun test-claude-org-direct ()
  "Test org UI directly."
  (interactive)
  (require 'claude-stream-org-ui)
  (let* ((prompt "Tell me a joke")
         (command (format "claude -p \"%s\" --output-format stream-json --verbose"
                         (shell-quote-argument prompt))))
    (message "Running command: %s" command)
    (claude-stream-org-ui-create-process "test-direct" command)))

(defun test-claude-parse-sample-json ()
  "Test parsing a sample JSON line."
  (interactive)
  (require 'claude-stream-org-ui)
  (let* ((buffer (generate-new-buffer "*Test Org Parse*"))
         (sample-lines '("{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test123\",\"model\":\"claude-3\",\"permissionMode\":\"default\",\"tools\":[]}"
                        "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_001\",\"content\":[{\"type\":\"text\",\"text\":\"Hello! How can I help you today?\"}],\"stop_reason\":\"end_turn\"}}")))
    (with-current-buffer buffer
      (org-mode)
      (setq-local claude-stream-org--messages (make-hash-table :test 'equal))
      (setq-local claude-stream-org--active-tools (make-hash-table :test 'equal))
      (setq-local claude-stream-org--in-src-block nil)
      
      (insert "#+TITLE: Test Parsing\n\n")
      
      (dolist (line sample-lines)
        (let ((data (claude-stream-org--parse-json-line line)))
          (when data
            (save-excursion
              (claude-stream-org--render-event data))))))
    
    (switch-to-buffer buffer)))

(defun test-claude-check-command ()
  "Check what command is being built."
  (interactive)
  (let ((prompt "Test prompt"))
    (message "Command: %s" (usura-build-claude-command prompt))))

(defun test-claude-org-with-echo ()
  "Test with a simple echo to see if process works."
  (interactive)
  (let* ((buffer (generate-new-buffer "*Claude Echo Test*"))
         (proc (start-process-shell-command 
                "echo-test" 
                buffer 
                "echo '{\"type\":\"system\",\"subtype\":\"init\"}'; sleep 1; echo '{\"type\":\"assistant\",\"message\":{\"id\":\"msg_001\",\"content\":[{\"type\":\"text\",\"text\":\"Test message\"}]}}'")))
    
    (with-current-buffer buffer
      (org-mode)
      (setq-local claude-stream-org--buffer "")
      (setq-local claude-stream-org--messages (make-hash-table :test 'equal))
      (setq-local claude-stream-org--active-tools (make-hash-table :test 'equal))
      (setq-local claude-stream-org--render-timer nil)
      (setq-local claude-stream-org--render-queue nil)
      (setq-local claude-stream-org--in-src-block nil)
      
      (insert "#+TITLE: Echo Test\n\n"))
    
    (set-process-filter proc #'claude-stream-org--process-filter)
    (switch-to-buffer buffer)))

(provide 'test-claude-org-debug)
;;; test-claude-org-debug.el ends here