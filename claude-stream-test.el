;;; claude-stream-test.el --- Test Claude stream with proper flags -*- lexical-binding: t; -*-

(require 'claude-stream-parser)
(require 'claude-stream-usage)

(defun claude-stream-run-advice (prompt)
  "Run Claude with advice PROMPT using stream JSON format."
  (interactive "sAdvice prompt: ")
  (let ((command (format "claude -p \"/advice %s\" --output-format stream-json --verbose" 
                         (shell-quote-argument prompt))))
    (message "[DEBUG] Full command: %s" command)
    (claude-stream-create-process
     "claude-advice"
     command
     "*Claude Advice Stream*"
     (lambda (data)
       (message "[DEBUG] Complete message callback triggered")
       (with-current-buffer "*Claude Advice Stream*"
         (goto-char (point-max))
         (insert "\n\n=== Formatted Message ===\n")
         (insert (claude-format-message data))
         (insert "\n"))))))

(defun claude-stream-test-with-file ()
  "Test with a pre-recorded Claude stream file."
  (interactive)
  (let ((test-file (expand-file-name "~/claude-test-stream.json")))
    (if (file-exists-p test-file)
        (let ((command (format "cat %s" test-file)))
          (message "[DEBUG] Reading from file: %s" test-file)
          (claude-stream-create-process
           "claude-file-test"
           command
           "*Claude File Test*"
           (lambda (data)
             (message "[DEBUG] Got complete message from file"))))
      (message "Test file not found: %s" test-file))))

(defun claude-stream-debug-raw ()
  "Show raw output from claude command for debugging."
  (interactive)
  (let* ((prompt (read-string "Prompt: "))
         (command (format "claude -p \"%s\" --output-format stream-json --verbose 2>&1" 
                          (shell-quote-argument prompt)))
         (buffer (get-buffer-create "*Claude Raw Debug*")))
    (message "[DEBUG] Raw command: %s" command)
    (with-current-buffer buffer
      (erase-buffer)
      (insert (format "Command: %s\n" command))
      (insert "==================\n\n"))
    (display-buffer buffer)
    (let ((proc (start-process-shell-command
                 "claude-raw"
                 buffer
                 command)))
      (set-process-sentinel 
       proc 
       (lambda (p e)
         (with-current-buffer buffer
           (goto-char (point-max))
           (insert (format "\n\n==================\nProcess ended: %s" e))))))))

;; Quick test to verify stream format
(defun claude-stream-verify-format ()
  "Verify Claude is outputting stream JSON format."
  (interactive)
  (let ((command "claude -p \"hello\" --output-format stream-json --verbose"))
    (message "[DEBUG] Testing command: %s" command)
    (shell-command command "*Claude Format Test*")))

;; Interactive structured view test
(defun claude-test-structured ()
  "Interactively test structured view with any prompt."
  (interactive)
  (let ((prompt (read-string "Enter Claude prompt: " "/advice ")))
    (claude-stream-structured-view prompt)))

;; Simple test with common prompts
(defun claude-test-common-prompts ()
  "Test with common prompts that trigger different behaviors."
  (interactive)
  (let ((prompts '("hello"
                   "/advice should I learn rust or go?"
                   "explain quantum computing"
                   "/project:multi-mind climate change")))
    (let ((choice (completing-read "Choose a test prompt: " prompts)))
      (claude-stream-structured-view choice))))

(provide 'claude-stream-test)
;;; claude-stream-test.el ends here