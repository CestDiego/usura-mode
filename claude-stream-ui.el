;;; claude-stream-ui.el --- High-performance Claude stream UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Unified, performant stream processor with advanced text-based UI
;; Uses Emacs 29+ async features and efficient rendering

;;; Code:

(require 'json)
(require 'subr-x)
(require 'cl-lib)

;; Performance optimizations
(defcustom claude-stream-chunk-size 4096
  "Buffer chunk size for stream processing."
  :type 'integer
  :group 'claude-stream)

(defcustom claude-stream-render-delay 0.05
  "Delay in seconds between render updates to prevent flicker."
  :type 'float
  :group 'claude-stream)

;; UI Templates as text blocks
(defconst claude-stream-ui-templates
  '((session-init . "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ 🚀 Claude Session Started                                                   ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ Session: %s                                    ┃
┃ Model:   %s                                                  ┃
┃ Mode:    %s                                                        ┃
┃ Tools:   %d available                                                       ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛")
    
    (assistant-header . "
╔══════════════════════════════════════════════════════════════════════════════╗
║ 🤖 Assistant Response                                          [%s] ║
╚══════════════════════════════════════════════════════════════════════════════╝")
    
    (text-content . "
%s")
    
    (thinking-header . "
┌─💭 Thinking ─────────────────────────────────────────────────────────────────┐")
    (thinking-content . "│ %s")
    (thinking-footer . "└──────────────────────────────────────────────────────────────────────────────┘")
    
    (tool-use-header . "
┌─🔧 Tool: %s ────────────────────────────────────────────────┤ID: %s├─┐")
    (tool-use-input . "│ ▸ %s: %s")
    (tool-use-footer . "└──────────────────────────────────────────────────────────────────────────────┘")
    
    (tool-result-success . "
✅ Tool Result [%s] ───────────────────────────────────────────────────────────┐
%s
└──────────────────────────────────────────────────────────────────────────────┘")
    
    (tool-result-error . "
❌ Tool Error [%s] ────────────────────────────────────────────────────────────┐
%s
└──────────────────────────────────────────────────────────────────────────────┘")
    
    (final-result . "
╔══════════════════════════════════════════════════════════════════════════════╗
║ 📊 Final Result                                                              ║
╠══════════════════════════════════════════════════════════════════════════════╣
║ Duration: %s ms (API: %s ms)                                      ║
║ Turns: %d | Cost: $%.6f                                                      ║
║ Tokens: %d in, %d out (Cache: %d created, %d read)                          ║
╚══════════════════════════════════════════════════════════════════════════════╝")
    
    (progress-indicator . "⣾⣽⣻⢿⡿⣟⣯⣷")))

;; Stream state management
(defvar-local claude-stream--buffer "")
(defvar-local claude-stream--messages (make-hash-table :test 'equal))
(defvar-local claude-stream--active-tools (make-hash-table :test 'equal))
(defvar-local claude-stream--render-timer nil)
(defvar-local claude-stream--render-queue nil)
(defvar-local claude-stream--progress-position 0)

;; High-performance JSON parser
(defun claude-stream--parse-json-line (line)
  "Parse LINE as JSON with error handling."
  (condition-case err
      (let ((json-object-type 'alist)
            (json-array-type 'list)
            (json-key-type 'string))
        (json-read-from-string line))
    (error
     (message "[Stream] JSON parse error: %s" err)
     nil)))

;; Async process filter with buffering
(defun claude-stream--process-filter (proc string)
  "High-performance filter for PROC receiving STRING."
  (with-current-buffer (process-buffer proc)
    (setq claude-stream--buffer (concat claude-stream--buffer string))
    
    ;; Process complete lines
    (let ((lines nil))
      (while (string-match "\n" claude-stream--buffer)
        (push (substring claude-stream--buffer 0 (match-beginning 0)) lines)
        (setq claude-stream--buffer (substring claude-stream--buffer (match-end 0))))
      
      ;; Batch process lines
      (when lines
        (claude-stream--batch-process-lines (nreverse lines))))))

(defun claude-stream--batch-process-lines (lines)
  "Process LINES in batch for efficiency."
  (dolist (line lines)
    (when (> (length line) 0)
      (when-let ((data (claude-stream--parse-json-line line)))
        (claude-stream--queue-render data)))))

;; Render queue for batched updates
(defun claude-stream--queue-render (data)
  "Queue DATA for rendering."
  (push data claude-stream--render-queue)
  (unless claude-stream--render-timer
    (setq claude-stream--render-timer
          (run-with-timer claude-stream-render-delay nil
                          #'claude-stream--flush-render-queue))))

(defun claude-stream--flush-render-queue ()
  "Flush all queued renders at once."
  (setq claude-stream--render-timer nil)
  (let ((inhibit-redisplay t)
        (queue (nreverse claude-stream--render-queue)))
    (setq claude-stream--render-queue nil)
    (save-excursion
      (dolist (data queue)
        (claude-stream--render-event data))))
  (redisplay))

;; Event rendering with templates
(defun claude-stream--render-event (data)
  "Render DATA event using appropriate template."
  (let ((type (alist-get "type" data nil nil #'string=)))
    (cond
     ((string= type "system")
      (claude-stream--render-system data))
     ((string= type "assistant")
      (claude-stream--render-assistant data))
     ((string= type "user")
      (claude-stream--render-user data))
     ((string= type "result")
      (claude-stream--render-result data)))))

(defun claude-stream--render-system (data)
  "Render system DATA."
  (let ((subtype (alist-get "subtype" data nil nil #'string=)))
    (when (string= subtype "init")
      (goto-char (point-max))
      (insert (format (alist-get 'session-init claude-stream-ui-templates)
                      (substring (alist-get "session_id" data nil nil #'string=) 0 8)
                      (alist-get "model" data nil nil #'string=)
                      (alist-get "permissionMode" data nil nil #'string=)
                      (length (alist-get "tools" data nil nil #'string=)))
              "\n\n"))))

(defun claude-stream--render-assistant (data)
  "Render assistant DATA."
  (let* ((message (alist-get "message" data nil nil #'string=))
         (msg-id (alist-get "id" message nil nil #'string=))
         (content (alist-get "content" message nil nil #'string=))
         (stop-reason (alist-get "stop_reason" message nil nil #'string=))
         (existing (gethash msg-id claude-stream--messages)))
    
    ;; First message with this ID
    (unless existing
      (goto-char (point-max))
      (insert (format (alist-get 'assistant-header claude-stream-ui-templates)
                      (if stop-reason "Complete" "Streaming...")))
      (puthash msg-id (point-marker) claude-stream--messages))
    
    ;; Render content items
    (dolist (item content)
      (when (listp item)
        (claude-stream--render-content-item item msg-id)))))

(defun claude-stream--render-content-item (item msg-id)
  "Render content ITEM for MSG-ID."
  (let ((content-type (alist-get "type" item nil nil #'string=)))
    (goto-char (point-max))
    (cond
     ;; Text content
     ((string= content-type "text")
      (let ((text (alist-get "text" item nil nil #'string=)))
        (insert (format (alist-get 'text-content claude-stream-ui-templates) text))))
     
     ;; Thinking content
     ((string= content-type "thinking")
      (let ((thinking (alist-get "thinking" item nil nil #'string=)))
        (insert (alist-get 'thinking-header claude-stream-ui-templates) "\n")
        (dolist (line (split-string thinking "\n"))
          (insert (format (alist-get 'thinking-content claude-stream-ui-templates) 
                          (claude-stream--wrap-text line 76)) "\n"))
        (insert (alist-get 'thinking-footer claude-stream-ui-templates))))
     
     ;; Tool use
     ((string= content-type "tool_use")
      (let ((tool-id (alist-get "id" item nil nil #'string=))
            (tool-name (alist-get "name" item nil nil #'string=))
            (tool-input (alist-get "input" item nil nil #'string=)))
        ;; Track active tool
        (puthash tool-id (list :name tool-name :start (current-time)) 
                 claude-stream--active-tools)
        ;; Render header
        (insert (format (alist-get 'tool-use-header claude-stream-ui-templates)
                        tool-name (substring tool-id -8)))
        (insert "\n")
        ;; Render inputs
        (when (listp tool-input)
          (dolist (field tool-input)
            (when (consp field)
              (insert (format (alist-get 'tool-use-input claude-stream-ui-templates)
                              (car field)
                              (claude-stream--format-value (cdr field) 60))
                      "\n"))))
        (insert (alist-get 'tool-use-footer claude-stream-ui-templates)))))))

(defun claude-stream--render-user (data)
  "Render user DATA (tool results)."
  (let* ((message (alist-get "message" data nil nil #'string=))
         (content (alist-get "content" message nil nil #'string=)))
    (dolist (item content)
      (when (listp item)
        (let ((tool-use-id (alist-get "tool_use_id" item nil nil #'string=))
              (tool-result (alist-get "tool_result" item nil nil #'string=))
              (is-error (alist-get "is_error" item)))
          (when tool-result
            (goto-char (point-max))
            (let* ((tool-info (gethash tool-use-id claude-stream--active-tools))
                   (tool-name (plist-get tool-info :name))
                   (content-text (alist-get "content" tool-result nil nil #'string=))
                   (template (if is-error 'tool-result-error 'tool-result-success)))
              (insert (format (alist-get template claude-stream-ui-templates)
                              (or tool-name (substring tool-use-id -8))
                              (claude-stream--format-result content-text))
                      "\n")
              ;; Clear active tool
              (remhash tool-use-id claude-stream--active-tools))))))))

(defun claude-stream--render-result (data)
  "Render final result DATA."
  (goto-char (point-max))
  (let ((duration (alist-get "duration_ms" data))
        (api-duration (alist-get "duration_api_ms" data))
        (turns (alist-get "num_turns" data))
        (cost (alist-get "total_cost_usd" data))
        (usage (alist-get "usage" data)))
    (insert (format (alist-get 'final-result claude-stream-ui-templates)
                    duration api-duration turns cost
                    (alist-get "input_tokens" usage)
                    (alist-get "output_tokens" usage)
                    (alist-get "cache_creation_input_tokens" usage)
                    (alist-get "cache_read_input_tokens" usage))
            "\n\n")
    ;; Add the actual result text
    (insert (alist-get "result" data nil nil #'string=))))

;; Helper functions
(defun claude-stream--wrap-text (text max-width)
  "Wrap TEXT to MAX-WIDTH characters."
  (if (> (length text) max-width)
      (concat (substring text 0 max-width) "...")
    text))

(defun claude-stream--format-value (value max-length)
  "Format VALUE for display, truncating to MAX-LENGTH."
  (let ((str (format "%S" value)))
    (if (> (length str) max-length)
        (concat (substring str 0 max-length) "...")
      str)))

(defun claude-stream--format-result (content)
  "Format result CONTENT for display."
  (if (stringp content)
      (let ((lines (split-string content "\n")))
        (mapconcat (lambda (line) (concat "│ " line))
                   (if (> (length lines) 20)
                       (append (seq-take lines 20) '("│ ..."))
                     lines)
                   "\n"))
    (format "│ %S" content)))

;; Progress indicator for long operations
(defun claude-stream--show-progress ()
  "Show progress indicator."
  (let ((chars (alist-get 'progress-indicator claude-stream-ui-templates)))
    (setq claude-stream--progress-position 
          (% (1+ claude-stream--progress-position) (length chars)))
    (nth claude-stream--progress-position chars)))

;; Main entry point
(defun claude-stream-ui-create-process (name command &optional callback)
  "Create high-performance Claude stream process NAME with COMMAND.
Optional CALLBACK is called on complete messages."
  (let* ((buffer (generate-new-buffer (format "*Claude: %s*" name)))
         (proc (start-process-shell-command name buffer command)))
    
    (with-current-buffer buffer
      ;; Initialize buffer-local state
      (setq claude-stream--buffer ""
            claude-stream--messages (make-hash-table :test 'equal)
            claude-stream--active-tools (make-hash-table :test 'equal)
            claude-stream--render-timer nil
            claude-stream--render-queue nil
            claude-stream--progress-position 0)
      
      ;; Set up text properties for better visuals
      (setq-local line-spacing 0.1)
      (setq-local truncate-lines nil)
      (setq-local word-wrap t)
      
      ;; Custom face definitions
      (face-remap-add-relative 'default :height 1.0)
      
      (goto-char (point-min))
      (insert "Initializing Claude stream...\n\n"))
    
    (set-process-filter proc #'claude-stream--process-filter)
    (set-process-sentinel proc
                          (lambda (p e)
                            (with-current-buffer (process-buffer p)
                              (goto-char (point-max))
                              (insert "\n\n[Stream ended: " e "]\n"))))
    
    ;; Display buffer with nice window settings
    (let ((window (display-buffer buffer 
                                  '(display-buffer-pop-up-window
                                    (window-height . 0.5)))))
      (set-window-dedicated-p window t))
    
    proc))

;; Interactive commands
(defun claude-stream-ui-test ()
  "Test the Claude stream UI."
  (interactive)
  (let ((prompt (read-string "Claude prompt: " "/advice ")))
    (claude-stream-ui-create-process
     "test"
     (format "claude -p \"%s\" --output-format stream-json --verbose"
             (shell-quote-argument prompt)))))

(provide 'claude-stream-ui)
;;; claude-stream-ui.el ends here