;;; claude-stream-usage.el --- Example usage of Claude stream parser -*- lexical-binding: t; -*-

(require 'claude-stream-parser)

;; Example: Parse Claude stream from a file or command
(defun parse-claude-stream-file (file)
  "Parse Claude stream from FILE."
  (interactive "fClaude stream file: ")
  (claude-stream-create-process
   "claude-file-stream"
   (format "cat %s" file)
   "*Claude Stream*"
   (lambda (data)
     (message "Message complete: %S" data))))

;; Example: Parse real-time Claude API stream
(defun parse-claude-api-stream (api-command)
  "Parse Claude API stream from API-COMMAND."
  (interactive "sClaude API command: ")
  (claude-stream-create-process
   "claude"
   api-command
   "*Claude API Output*"
   (lambda (data)
     ;; Custom callback to handle completed messages
     (let* ((message (alist-get "message" data nil nil #'string=))
            (content (alist-get "content" message nil nil #'string=)))
       (dolist (item content)
         (when (and (listp item)
                   (string= (alist-get "type" item nil nil #'string=) "tool_use"))
           (message "Tool completed: %s" 
                   (alist-get "name" item nil nil #'string=))))))))

;; Example: Create a more structured view
(defun claude-stream-structured-view (input)
  "Create a structured view of Claude stream output from INPUT."
  (interactive "sEnter prompt for Claude: ")
  (let ((output-buffer (get-buffer-create "*Claude Structured View*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert "Claude Stream Structured View\n")
      (insert "=============================\n")
      (insert (format "Prompt: %s\n" input))
      (insert "=============================\n\n"))
    
    (claude-stream-create-process
     "claude-structured"
     (format "claude -p \"%s\" --output-format stream-json --verbose" 
             (shell-quote-argument input))
     "*Claude Raw Stream*"
     (lambda (data)
       (with-current-buffer output-buffer
         (goto-char (point-max))
         (insert (claude-format-message data))
         (insert "\n---\n\n"))))))

(defun claude-format-message (data)
  "Format Claude message DATA exhaustively for display."
  (let* ((type (alist-get "type" data nil nil #'string=))
         (formatted (format "╔═══ Message Type: %s ═══╗\n" (upcase type))))
    
    (cond
     ;; System messages
     ((string= type "system")
      (let ((subtype (alist-get "subtype" data nil nil #'string=)))
        (setq formatted (concat formatted (format "║ Subtype: %s\n" subtype)))
        (when (string= subtype "init")
          (setq formatted (concat formatted 
                                (format "║ Session ID: %s\n" (alist-get "session_id" data nil nil #'string=))
                                (format "║ CWD: %s\n" (alist-get "cwd" data nil nil #'string=))
                                (format "║ Model: %s\n" (alist-get "model" data nil nil #'string=))
                                (format "║ Permission Mode: %s\n" (alist-get "permissionMode" data nil nil #'string=))
                                (format "║ API Key Source: %s\n" (alist-get "apiKeySource" data nil nil #'string=))
                                (format "║ Tools: %s\n" (claude-format-list (alist-get "tools" data nil nil #'string=)))
                                (format "║ MCP Servers: %s\n" (alist-get "mcp_servers" data nil nil #'string=)))))))
     
     ;; Assistant messages
     ((string= type "assistant")
      (let* ((message (alist-get "message" data nil nil #'string=))
             (msg-id (alist-get "id" message nil nil #'string=))
             (msg-type (alist-get "type" message nil nil #'string=))
             (role (alist-get "role" message nil nil #'string=))
             (model (alist-get "model" message nil nil #'string=))
             (content (alist-get "content" message nil nil #'string=))
             (stop-reason (alist-get "stop_reason" message nil nil #'string=))
             (stop-sequence (alist-get "stop_sequence" message nil nil #'string=))
             (usage (alist-get "usage" message nil nil #'string=)))
        
        (setq formatted (concat formatted
                              (format "║ Message ID: %s\n" msg-id)
                              (format "║ Message Type: %s\n" msg-type)
                              (format "║ Role: %s\n" role)
                              (format "║ Model: %s\n" model)
                              (format "║ Stop Reason: %s\n" (or stop-reason "null (streaming)"))
                              (format "║ Stop Sequence: %s\n" (or stop-sequence "null"))))
        
        ;; Usage information
        (when usage
          (setq formatted (concat formatted "║ Usage:\n"))
          (dolist (key '("input_tokens" "output_tokens" 
                        "cache_creation_input_tokens" "cache_read_input_tokens"
                        "service_tier"))
            (when-let ((value (alist-get key usage nil nil #'string=)))
              (setq formatted (concat formatted (format "║   %s: %s\n" key value))))))
        
        ;; Parent tool use ID and session ID
        (when-let ((parent-id (alist-get "parent_tool_use_id" data nil nil #'string=)))
          (setq formatted (concat formatted (format "║ Parent Tool Use ID: %s\n" parent-id))))
        (when-let ((session-id (alist-get "session_id" data nil nil #'string=)))
          (setq formatted (concat formatted (format "║ Session ID: %s\n" session-id))))
        
        ;; Content items
        (when content
          (setq formatted (concat formatted "║\n║ Content Items:\n"))
          (dolist (item content)
            (when (listp item)
              (setq formatted (concat formatted (claude-format-content-item item))))))))
     
     ;; User messages
     ((string= type "user")
      (let* ((message (alist-get "message" data nil nil #'string=))
             (role (alist-get "role" message nil nil #'string=))
             (content (alist-get "content" message nil nil #'string=)))
        (setq formatted (concat formatted (format "║ Role: %s\n" role)))
        (when-let ((parent-id (alist-get "parent_tool_use_id" data nil nil #'string=)))
          (setq formatted (concat formatted (format "║ Parent Tool Use ID: %s\n" parent-id))))
        (when-let ((session-id (alist-get "session_id" data nil nil #'string=)))
          (setq formatted (concat formatted (format "║ Session ID: %s\n" session-id))))
        
        ;; User content (usually tool results)
        (when content
          (setq formatted (concat formatted "║\n║ Content:\n"))
          (dolist (item content)
            (when (listp item)
              (let ((tool-use-id (alist-get "tool_use_id" item nil nil #'string=))
                    (tool-result (alist-get "tool_result" item nil nil #'string=)))
                (when tool-result
                  (setq formatted (concat formatted 
                                        (format "║ ┌─ Tool Result (ID: %s)\n" tool-use-id)))
                  (let ((result-content (alist-get "content" tool-result nil nil #'string=)))
                    (if (stringp result-content)
                        (setq formatted (concat formatted 
                                              (claude-format-multiline result-content "║ │ ")))
                      ;; Handle array content (like text blocks)
                      (dolist (content-item result-content)
                        (when (listp content-item)
                          (let ((text (alist-get "text" content-item nil nil #'string=)))
                            (when text
                              (setq formatted (concat formatted 
                                                    (claude-format-multiline text "║ │ "))))))))))))))))))
    
    (concat formatted "╚" (make-string 50 ?═) "╝")))

(defun claude-format-content-item (item)
  "Format a single content ITEM."
  (let ((content-type (alist-get "type" item nil nil #'string=))
        (formatted ""))
    (cond
     ;; Text content
     ((string= content-type "text")
      (let ((text (alist-get "text" item nil nil #'string=)))
        (setq formatted (concat formatted 
                              (format "║ ├─ [TEXT]\n")
                              (claude-format-multiline text "║ │  ")))))
     
     ;; Tool use
     ((string= content-type "tool_use")
      (let ((tool-id (alist-get "id" item nil nil #'string=))
            (tool-name (alist-get "name" item nil nil #'string=))
            (tool-input (alist-get "input" item nil nil #'string=)))
        (setq formatted (concat formatted
                              (format "║ ├─ [TOOL USE]\n")
                              (format "║ │  ID: %s\n" tool-id)
                              (format "║ │  Name: %s\n" tool-name)
                              "║ │  Input:\n"))
        ;; Format tool input fields
        (when (listp tool-input)
          (dolist (field tool-input)
            (when (consp field)
              (let ((key (car field))
                    (value (cdr field)))
                (setq formatted (concat formatted 
                                      (format "║ │    %s: %s\n" 
                                             key 
                                             (claude-truncate-value value 100))))))))))
     
     ;; Thinking content
     ((string= content-type "thinking")
      (let ((thinking-text (alist-get "thinking" item nil nil #'string=))
            (signature (alist-get "signature" item nil nil #'string=)))
        (setq formatted (concat formatted
                              (format "║ ├─ [THINKING]\n")))
        (when thinking-text
          (setq formatted (concat formatted
                                (claude-format-multiline thinking-text "║ │  " 500))))
        (when signature
          (setq formatted (concat formatted
                                (format "║ │  Signature: %s...\n" 
                                       (substring signature 0 (min 20 (length signature)))))))))
     
     ;; Other content types
     (t
      (setq formatted (concat formatted
                            (format "║ ├─ [%s] %S\n" (upcase content-type) item)))))
    
    formatted))

(defun claude-format-multiline (text prefix &optional max-length)
  "Format multiline TEXT with PREFIX, optionally truncating to MAX-LENGTH."
  (let ((truncated (if (and max-length (> (length text) max-length))
                       (concat (substring text 0 max-length) "...")
                     text)))
    (mapconcat (lambda (line) (concat prefix line))
               (split-string truncated "\n")
               "\n")))

(defun claude-truncate-value (value max-length)
  "Truncate VALUE to MAX-LENGTH for display."
  (let ((str (format "%S" value)))
    (if (> (length str) max-length)
        (concat (substring str 0 max-length) "...")
      str)))

(defun claude-format-list (lst)
  "Format LST as a comma-separated string."
  (if (listp lst)
      (mapconcat #'identity lst ", ")
    (format "%s" lst)))

;; Test with sample data
(defun test-claude-parser-with-sample ()
  "Test the parser with sample Claude stream data."
  (interactive)
  (let ((sample-file (make-temp-file "claude-sample" nil ".json")))
    ;; Write sample data
    (with-temp-file sample-file
      (insert "{\"type\":\"system\",\"subtype\":\"init\",\"session_id\":\"test-123\",\"model\":\"claude-3\"}\n")
      (insert "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_123\",\"content\":[{\"type\":\"text\",\"text\":\"Hello!\"}]}}\n")
      (insert "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_123\",\"content\":[{\"type\":\"tool_use\",\"name\":\"Task\",\"id\":\"tool_456\"}],\"stop_reason\":\"tool_use\"}}\n"))
    
    ;; Parse it
    (parse-claude-stream-file sample-file)
    
    ;; Clean up
    (run-at-time "5 sec" nil (lambda () (delete-file sample-file)))))

(provide 'claude-stream-usage)
;;; claude-stream-usage.el ends here
