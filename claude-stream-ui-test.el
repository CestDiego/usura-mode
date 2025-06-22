;;; claude-stream-ui-test.el --- Test and demo for Claude stream UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Comprehensive test suite showing all UI capabilities

;;; Code:

(require 'claude-stream-ui)

;; Test with different event types
(defun claude-stream-ui-test-all-events ()
  "Test UI with mock data containing all event types."
  (interactive)
  (let* ((buffer (get-buffer-create "*Claude UI Test*"))
         (test-data 
          '(;; System init
            "{\"type\":\"system\",\"subtype\":\"init\",\"cwd\":\"/home/user\",\"session_id\":\"test-123-abc\",\"tools\":[\"Task\",\"Bash\",\"Read\",\"Write\"],\"model\":\"claude-opus-4\",\"permissionMode\":\"default\",\"apiKeySource\":\"env\"}"
            
            ;; Assistant with text
            "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_001\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4\",\"content\":[{\"type\":\"text\",\"text\":\"I'll help you analyze this complex situation using multiple perspectives.\"}],\"stop_reason\":null}}"
            
            ;; Assistant with thinking
            "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_001\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"The user is asking about a complex decision. I should consider multiple angles:\\n1. Technical feasibility\\n2. Business impact\\n3. User experience\\n4. Long-term sustainability\\n\\nThis requires careful analysis.\",\"signature\":\"ABC123XYZ789\"}],\"stop_reason\":null}}"
            
            ;; Assistant with tool use
            "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_001\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tool_123\",\"name\":\"WebSearch\",\"input\":{\"query\":\"latest Emacs packages for AI integration\",\"allowed_domains\":[\"github.com\",\"melpa.org\"]}}],\"stop_reason\":null}}"
            
            ;; Another tool use
            "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_001\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tool_124\",\"name\":\"Read\",\"input\":{\"file_path\":\"/Users/test/config.el\",\"limit\":100}}],\"stop_reason\":\"tool_use\"}}"
            
            ;; Tool result (success)
            "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"tool_use_id\":\"tool_123\",\"type\":\"tool_result\",\"content\":\"Found 5 relevant packages:\\n1. gptel - Multi-backend LLM client\\n2. ellama - Ollama integration\\n3. org-ai - AI in Org mode\\n4. copilot.el - GitHub Copilot\\n5. chatgpt-shell - ChatGPT interface\"}]}}"
            
            ;; Tool result (error)
            "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"tool_use_id\":\"tool_124\",\"type\":\"tool_result\",\"is_error\":true,\"content\":\"Error: File not found\"}]}}"
            
            ;; Assistant continuation
            "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_001\",\"content\":[{\"type\":\"text\",\"text\":\"Based on my search, here are the most relevant Emacs packages for AI integration:\\n\\n**gptel** stands out as the most flexible option, supporting multiple backends including OpenAI, Anthropic, and local models.\"}],\"stop_reason\":\"end_turn\"}}"
            
            ;; Final result
            "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"duration_ms\":3456,\"duration_api_ms\":2890,\"num_turns\":4,\"total_cost_usd\":0.0234,\"usage\":{\"input_tokens\":567,\"output_tokens\":234,\"cache_creation_input_tokens\":1000,\"cache_read_input_tokens\":500},\"result\":\"Analysis complete. Recommended gptel for maximum flexibility.\"}")))
    
    (with-current-buffer buffer
      (erase-buffer)
      (claude-stream-ui-mode))
    
    ;; Simulate streaming
    (dolist (line test-data)
      (with-current-buffer buffer
        (claude-stream--queue-render (claude-stream--parse-json-line line)))
      (sit-for 0.5))
    
    (display-buffer buffer)))

;; Test real commands
(defun claude-stream-ui-test-advice ()
  "Test with /advice command."
  (interactive)
  (claude-stream-ui-create-process
   "advice"
   "claude -p \"/advice should I switch from Emacs to VS Code?\" --output-format stream-json --verbose"))

(defun claude-stream-ui-test-multi-mind ()
  "Test with multi-mind command."
  (interactive)
  (claude-stream-ui-create-process
   "multi-mind"
   "claude -p \"/project:multi-mind future of text editors\" --output-format stream-json --verbose"))

;; Test edge cases
(defun claude-stream-ui-test-edge-cases ()
  "Test UI with edge cases and special characters."
  (interactive)
  (let ((test-cases
         '(;; Unicode and emojis
           "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_002\",\"content\":[{\"type\":\"text\",\"text\":\"Testing unicode: 你好世界 🌍 λ→∞ ∀x∈ℝ\"}]}}"
           
           ;; Very long text
           "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_003\",\"content\":[{\"type\":\"text\",\"text\":\"This is a very long response that goes on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and on and should be wrapped properly.\"}]}}"
           
           ;; Nested JSON in tool input
           "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_004\",\"content\":[{\"type\":\"tool_use\",\"id\":\"tool_125\",\"name\":\"Task\",\"input\":{\"description\":\"Complex task\",\"config\":{\"nested\":{\"deep\":\"value\"}},\"array\":[1,2,3]}}]}}"
           
           ;; Multiple content types in one message
           "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_005\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"First, let me think...\"},{\"type\":\"text\",\"text\":\"Here's my response:\"},{\"type\":\"tool_use\",\"id\":\"tool_126\",\"name\":\"Bash\",\"input\":{\"command\":\"ls -la\"}}]}}")))
    
    (let ((buffer (get-buffer-create "*Claude UI Edge Cases*")))
      (with-current-buffer buffer
        (erase-buffer))
      
      (dolist (line test-cases)
        (with-current-buffer buffer
          (claude-stream--queue-render (claude-stream--parse-json-line line)))
        (sit-for 0.3))
      
      (display-buffer buffer))))

;; Performance test
(defun claude-stream-ui-performance-test ()
  "Test UI performance with rapid events."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude UI Performance*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Performance Test - 100 rapid events\n\n"))
    
    ;; Generate 100 events rapidly
    (dotimes (i 100)
      (let ((event (format "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_%03d\",\"content\":[{\"type\":\"text\",\"text\":\"Event %d: Processing...\"}]}}"
                           i i)))
        (with-current-buffer buffer
          (claude-stream--queue-render (claude-stream--parse-json-line event)))))
    
    (display-buffer buffer)
    (message "Performance test complete - check render quality")))

;; Interactive demo mode
(defun claude-stream-ui-demo ()
  "Interactive demo showcasing all UI features."
  (interactive)
  (let ((choices '(("All Events" . claude-stream-ui-test-all-events)
                   ("Real /advice" . claude-stream-ui-test-advice)
                   ("Multi-mind" . claude-stream-ui-test-multi-mind)
                   ("Edge Cases" . claude-stream-ui-test-edge-cases)
                   ("Performance" . claude-stream-ui-performance-test)
                   ("Custom Prompt" . claude-stream-ui-test))))
    (let ((choice (completing-read "Choose demo: " choices)))
      (funcall (alist-get choice choices nil nil #'string=)))))

;; Minor mode for enhanced viewing
(define-minor-mode claude-stream-ui-mode
  "Minor mode for Claude stream UI buffers."
  :lighter " Claude"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "q") 'quit-window)
            (define-key map (kbd "g") 'claude-stream-ui-test)
            (define-key map (kbd "c") 'claude-stream-ui-clear)
            (define-key map (kbd "TAB") 'claude-stream-ui-toggle-section)
            map))

(defun claude-stream-ui-clear ()
  "Clear the current buffer."
  (interactive)
  (erase-buffer))

(defun claude-stream-ui-toggle-section ()
  "Toggle visibility of section at point (future feature)."
  (interactive)
  (message "Section toggling not yet implemented"))

;; Test with advice file
(defun claude-stream-ui-test-advice-file ()
  "Test UI by parsing the advice file line by line."
  (interactive)
  (let ((advice-file "/Users/wayrapro/.config/doom/lisp/usura-mode/advice")
        (buffer (get-buffer-create "*Claude Stream from File*")))
    
    (if (file-exists-p advice-file)
        (progn
          (with-current-buffer buffer
            (erase-buffer)
            (claude-stream-ui-mode)
            (insert "Loading from advice file...\n\n"))
          
          ;; Read and process line by line
          (with-temp-buffer
            (insert-file-contents advice-file)
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))))
                (when (and (> (length line) 0)
                           (string-match "^{" line)) ; Only JSON lines
                  (with-current-buffer buffer
                    (condition-case err
                        (when-let ((data (claude-stream--parse-json-line line)))
                          (claude-stream--queue-render data)
                          ;; Force immediate render for file parsing
                          (claude-stream--flush-render-queue)
                          (sit-for 0.05)) ; Small delay to show streaming effect
                      (error
                       (insert (format "\n[Parse error: %s]\n" err)))))))
              (forward-line 1)))
          
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "\n\n[File parsing complete]\n"))
          
          (display-buffer buffer))
      (message "Advice file not found: %s" advice-file))))

;; Test with specific lines from advice file
(defun claude-stream-ui-test-advice-lines ()
  "Test UI with specific interesting lines from advice file."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude Advice Lines Test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (claude-stream-ui-mode))
    
    ;; Extract some interesting lines for testing
    (let ((test-lines
           '(;; System init
             "{\"type\":\"system\",\"subtype\":\"init\",\"cwd\":\"/Users/wayrapro/.config/doom/lisp/usura-mode\",\"session_id\":\"f580327e-8e75-4734-90dd-d94ac0b3d8d8\",\"tools\":[\"Task\",\"Bash\",\"Glob\",\"Grep\",\"LS\",\"exit_plan_mode\",\"Read\",\"Edit\",\"MultiEdit\",\"Write\",\"NotebookRead\",\"NotebookEdit\",\"WebFetch\",\"TodoRead\",\"TodoWrite\",\"WebSearch\"],\"mcp_servers\":[],\"model\":\"claude-opus-4-20250514\",\"permissionMode\":\"default\",\"apiKeySource\":\"none\"}"
             
             ;; Assistant text
             "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01J4qWoDxvCKUGLjppctBd2s\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"text\",\"text\":\"I'll help you explore this decision using the Internal Family Systems approach. Let me engage different parts of your internal system to understand their perspectives on leaving your job making emacs jokes.\"}],\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":3,\"cache_creation_input_tokens\":5127,\"cache_read_input_tokens\":10013,\"output_tokens\":1,\"service_tier\":\"standard\"}},\"parent_tool_use_id\":null,\"session_id\":\"f580327e-8e75-4734-90dd-d94ac0b3d8d8\"}"
             
             ;; Tool use
             "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01J4qWoDxvCKUGLjppctBd2s\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_01JTqnT4NHPgkcAL9Dj9ec5i\",\"name\":\"Task\",\"input\":{\"description\":\"Manager Part Analysis\",\"prompt\":\"As the Manager part in Internal Family Systems, analyze this situation: \\\"I want to quit my job making emacs jokes should i?\\\". Focus on planning, control, protection from future pain, and maintaining daily function. What preventive measures, structures, or controls would you suggest? Consider perfectionist tendencies, critical analysis, and worry-based protection strategies.\"}}],\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":3,\"cache_creation_input_tokens\":5127,\"cache_read_input_tokens\":10013,\"output_tokens\":1,\"service_tier\":\"standard\"}},\"parent_tool_use_id\":null,\"session_id\":\"f580327e-8e75-4734-90dd-d94ac0b3d8d8\"}"
             
             ;; Tool result
             "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"tool_use_id\":\"toolu_01JTqnT4NHPgkcAL9Dj9ec5i\",\"type\":\"tool_result\",\"content\":[{\"type\":\"text\",\"text\":\"I'll analyze this situation from an Internal Family Systems (IFS) Manager perspective, focusing on the protective and controlling aspects that managers typically embody.\\n\\n## Manager Analysis: Career Transition Concerns\\n\\n### Risk Assessment & Protective Measures\\n\\n**Financial Protection:**\\n- Do you have 6-12 months of emergency savings? This buffer protects against uncertainty\\n- Have you calculated your exact monthly expenses vs. current income from emacs jokes?\\n- What's your backup plan if joke revenue drops suddenly?\"}]}]},\"parent_tool_use_id\":null,\"session_id\":\"f580327e-8e75-4734-90dd-d94ac0b3d8d8\"}"
             
             ;; Final result
             "{\"type\":\"result\",\"subtype\":\"success\",\"is_error\":false,\"duration_ms\":62573,\"duration_api_ms\":127259,\"num_turns\":11,\"result\":\"## Internal Family Systems Analysis: Quitting Your Emacs Joke Job\\n\\n### Voices from Within:\\n\\n**Manager says:** You need structure and safety!\",\"session_id\":\"f580327e-8e75-4734-90dd-d94ac0b3d8d8\",\"total_cost_usd\":0.711477,\"usage\":{\"input_tokens\":857,\"cache_creation_input_tokens\":9314,\"cache_read_input_tokens\":25153,\"output_tokens\":1101,\"server_tool_use\":{\"web_search_requests\":0}}}")))
      
      (dolist (line test-lines)
        (with-current-buffer buffer
          (when-let ((data (claude-stream--parse-json-line line)))
            (claude-stream--queue-render data)
            (claude-stream--flush-render-queue)
            (sit-for 0.5)))))
    
    (display-buffer buffer)
    (message "Test complete - check the buffer for results")))

;; Utility to extract and analyze all event types from advice file
(defun claude-stream-ui-analyze-advice-file ()
  "Analyze the advice file to show all event types found."
  (interactive)
  (let ((advice-file "/Users/wayrapro/.config/doom/lisp/usura-mode/advice")
        (event-types (make-hash-table :test 'equal))
        (content-types (make-hash-table :test 'equal)))
    
    (when (file-exists-p advice-file)
      (with-temp-buffer
        (insert-file-contents advice-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (when (and (> (length line) 0)
                       (string-match "^{" line))
              (condition-case nil
                  (let* ((data (json-read-from-string line))
                         (type (alist-get 'type data)))
                    ;; Track event types
                    (puthash type (1+ (gethash type event-types 0)) event-types)
                    
                    ;; Track content types for assistant messages
                    (when (string= type "assistant")
                      (let ((content (alist-get 'content (alist-get 'message data))))
                        (dolist (item content)
                          (when (listp item)
                            (let ((content-type (alist-get 'type item)))
                              (puthash content-type 
                                       (1+ (gethash content-type content-types 0))
                                       content-types)))))))
                (error nil))))
          (forward-line 1)))
      
      ;; Display analysis
      (with-current-buffer (get-buffer-create "*Advice File Analysis*")
        (erase-buffer)
        (insert "=== Advice File Event Analysis ===\n\n")
        (insert "Event Types Found:\n")
        (maphash (lambda (k v) (insert (format "  %s: %d occurrences\n" k v))) 
                 event-types)
        (insert "\nContent Types Found in Assistant Messages:\n")
        (maphash (lambda (k v) (insert (format "  %s: %d occurrences\n" k v))) 
                 content-types)
        (display-buffer (current-buffer))))))

;; Export functions for testing
(provide 'claude-stream-ui-test)
;;; claude-stream-ui-test.el ends here