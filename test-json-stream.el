;;; test-json-stream.el --- Test JSON stream parsing -*- lexical-binding: t; -*-

(require 'json-stream-parser)

;; Test with your sample data
(defun test-json-stream ()
  "Test JSON stream parsing with sample data."
  (interactive)
  
  ;; Override callback for custom handling
  (defun json-stream-callback (data)
    "Custom callback to handle parsed JSON DATA."
    (let ((type (alist-get "type" data nil nil #'string=)))
      (cond
       ((string= type "assistant")
        (let* ((msg (alist-get "message" data nil nil #'string=))
               (content (alist-get "content" msg nil nil #'string=)))
          (message "Assistant message: %S" content)))
       (t
        (message "Other type: %s" type)))))
  
  ;; Simulate streaming data
  (with-temp-buffer
    (insert "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01NHd3mwbcJguL82UPJkPEtE\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_01BHtp6LqS2BS79C5RWJb9FN\",\"name\":\"Task\",\"input\":{\"description\":\"Exile part perspective\",\"prompt\":\"As the Exile part...\"}}],\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":9,\"cache_creation_input_tokens\":15549,\"cache_read_input_tokens\":0,\"output_tokens\":6,\"service_tier\":\"standard\"}},\"parent_tool_use_id\":null,\"session_id\":\"3e96ac68-5086-41f7-aeb3-9e8939abe95e\"}\n")
    
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties 
                   (line-beginning-position) 
                   (line-end-position))))
        (when (> (length line) 0)
          (json-stream-process-line line))
        (forward-line 1)))))

;; Alternative: Process from a file
(defun process-json-file (filename)
  "Process JSON stream from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties 
                   (line-beginning-position) 
                   (line-end-position))))
        (when (> (length line) 0)
          (json-stream-process-line line))
        (forward-line 1)))))

;; Usage:
;; M-x test-json-stream
;; Or: (process-json-file "/path/to/your/json-stream.txt")