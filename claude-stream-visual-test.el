;;; claude-stream-visual-test.el --- Visual UI test -*- lexical-binding: t; -*-

(require 'claude-stream-ui)

(defun claude-stream-visual-test ()
  "Visual test showing exactly what the UI looks like with real data."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude Stream Visual Test*")))
    (with-current-buffer buffer
      (erase-buffer)
      (setq claude-stream--buffer ""
            claude-stream--messages (make-hash-table :test 'equal)
            claude-stream--active-tools (make-hash-table :test 'equal)
            claude-stream--render-queue nil)
      
      ;; Process each line from the real conversation
      (let ((real-stream-lines
             '(;; System initialization
               "{\"type\":\"system\",\"subtype\":\"init\",\"cwd\":\"/Users/wayrapro/Documents/Projects/MCP/google-flights-mcp\",\"session_id\":\"8113cd23-f26a-4a12-bd19-a7c24d738130\",\"tools\":[\"Task\",\"Bash\",\"Glob\",\"Grep\",\"LS\",\"exit_plan_mode\",\"Read\",\"Edit\",\"MultiEdit\",\"Write\",\"NotebookRead\",\"NotebookEdit\",\"WebFetch\",\"TodoRead\",\"TodoWrite\",\"WebSearch\"],\"mcp_servers\":[],\"model\":\"claude-opus-4-20250514\",\"permissionMode\":\"default\",\"apiKeySource\":\"none\"}"
               
               ;; Assistant with thinking block
               "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01ECGbigJq6ktvfj7YogUFLT\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"The user is asking for advice about whether to open source their project. They work at Google and need to clear it with them, but they're torn because they think the project could be valuable to the developer community.\",\"signature\":\"Eu8GCkYIBBgC...\"}],\"stop_reason\":null}}"
               
               ;; Assistant with text
               "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01ECGbigJq6ktvfj7YogUFLT\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"text\",\"text\":\"I'll help you explore this decision using the Internal Family Systems approach, while also researching existing solutions. Let me engage multiple perspectives on your situation.\"}],\"stop_reason\":null}}"
               
               ;; Multiple tool uses
               "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01ECGbigJq6ktvfj7YogUFLT\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_016rZp31JErD9Ak1eQPiAv7P\",\"name\":\"Task\",\"input\":{\"description\":\"Manager part perspective\",\"prompt\":\"As the Manager part in Internal Family Systems, analyze this situation...\"}}],\"stop_reason\":null}}"
               
               "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01ECGbigJq6ktvfj7YogUFLT\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_0189y7fqgJVsvfFTv1HGUQrE\",\"name\":\"Task\",\"input\":{\"description\":\"Firefighter part perspective\",\"prompt\":\"As the Firefighter part in Internal Family Systems, respond to this situation...\"}}],\"stop_reason\":null}}"
               
               "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01ECGbigJq6ktvfj7YogUFLT\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_01PKY3rHPRaAYzXuUNGBmvkh\",\"name\":\"Task\",\"input\":{\"description\":\"Research LLM proxy solutions\",\"prompt\":\"Research existing LLM proxy solutions that allow developers to support multiple models...\"}}],\"stop_reason\":\"tool_use\"}}"
               
               ;; Tool results
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"tool_use_id\":\"toolu_01CoAim5VqZEz91xFjYjsrgR\",\"type\":\"tool_result\",\"content\":\"Web search results for query: \\\"LLM proxy gateway open source self-hosted multi-model\\\"\\n\\nI'll search for information about open source, self-hosted LLM proxy gateways that support multiple models.\\n\\nBased on my search results, here are several excellent open-source, self-hosted LLM proxy gateways...\"}]}]}"
               
               ;; Error result
               "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"tool_use_id\":\"toolu_01WkEh5dGyNaMJVjpsyZb26v\",\"type\":\"tool_result\",\"content\":\"File does not exist.\",\"is_error\":true}]},\"parent_tool_use_id\":\"toolu_011VLms88Aja7yVnL4wNVeoT\",\"session_id\":\"f580327e-8e75-4734-90dd-d94ac0b3d8d8\"}"
               
               ;; Final assistant summary
               "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01TnqibHtnP7tyoQuwqgSaLu\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"text\",\"text\":\"Found several similar Emacs packages:\\n\\n**Most relevant:**\\n- **gptel** - Flexible multi-model support with streaming\\n- **org-ai** - Best for multiple AI personas via org properties\\n- **ellama** - Local LLM focus via Ollama\\n- **llm** - Foundation package others build on\"}],\"stop_reason\":\"end_turn\"}}")))
        
        ;; Process each line
        (dolist (line real-stream-lines)
          (when-let ((data (claude-stream--parse-json-line line)))
            (claude-stream--queue-render data)
            (claude-stream--flush-render-queue)))))
    
    (display-buffer buffer)
    (with-current-buffer buffer
      (buffer-string))))

(provide 'claude-stream-visual-test)
;;; claude-stream-visual-test.el ends here