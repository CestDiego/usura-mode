;;; claude-stream-parser.el --- Parse Claude API streaming JSON output -*- lexical-binding: t; -*-

;;; Commentary:
;; Asynchronously parse Claude API newline-delimited JSON streams
;; Handles message continuations and different content types

;;; Code:

(require 'json)

(defvar-local claude-stream-buffer ""
  "Buffer for accumulating incomplete JSON data.")

(defvar-local claude-stream-messages (make-hash-table :test 'equal)
  "Hash table storing messages by ID for accumulation.")

(defvar-local claude-stream-output-buffer nil
  "Buffer where parsed content is displayed.")

(defvar-local claude-stream-callback nil
  "Callback function for processing parsed messages.")

(defun claude-stream-create-output-buffer (name)
  "Create or get output buffer with NAME."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (erase-buffer)
      (setq-local buffer-read-only nil))
    buf))

(defun claude-stream-process-filter (proc string)
  "Process filter for Claude JSON stream from PROC with STRING data."
  (with-current-buffer (process-buffer proc)
    (message "[DEBUG] Received %d bytes of data" (length string))
    (setq claude-stream-buffer (concat claude-stream-buffer string))
    
    ;; Process complete lines
    (while (string-match "\n" claude-stream-buffer)
      (let ((line (substring claude-stream-buffer 0 (match-beginning 0))))
        (setq claude-stream-buffer (substring claude-stream-buffer (match-end 0)))
        (when (> (length line) 0)
          (message "[DEBUG] Processing line: %s" (substring line 0 (min 100 (length line))))
          (claude-stream-process-line proc line))))))

(defun claude-stream-process-line (proc line)
  "Process a single LINE of JSON from PROC."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'string)
             (data (json-read-from-string line)))
        (claude-stream-handle-message proc data))
    (json-readtable-error
     (message "JSON parse error: %s" line))
    (error
     (message "Error processing: %s" err))))

(defun claude-stream-handle-message (proc data)
  "Handle parsed JSON DATA from PROC based on message type."
  (let ((type (alist-get "type" data nil nil #'string=)))
    (message "[DEBUG] Handling message type: %s" type)
    (cond
     ;; System messages
     ((string= type "system")
      (claude-stream-handle-system proc data))
     
     ;; Assistant messages (may be partial/streaming)
     ((string= type "assistant")
      (claude-stream-handle-assistant proc data))
     
     ;; User messages with tool results
     ((string= type "user")
      (claude-stream-handle-user proc data))
     
     ;; Unknown type
     (t
      (message "[DEBUG] Unknown message type: %s - Full data: %S" type data)))))

(defun claude-stream-handle-system (proc data)
  "Handle system message DATA from PROC."
  (let ((subtype (alist-get "subtype" data nil nil #'string=)))
    (when (string= subtype "init")
      ;; Write initialization info to output buffer
      (claude-stream-append-to-output 
       proc
       (format "=== Session Started ===\nSession ID: %s\nModel: %s\nTools: %s\n\n"
               (alist-get "session_id" data nil nil #'string=)
               (alist-get "model" data nil nil #'string=)
               (alist-get "tools" data nil nil #'string=))))))

(defun claude-stream-handle-assistant (proc data)
  "Handle assistant message DATA from PROC."
  (let* ((message (alist-get "message" data nil nil #'string=))
         (msg-id (alist-get "id" message nil nil #'string=))
         (content (alist-get "content" message nil nil #'string=))
         (stop-reason (alist-get "stop_reason" message nil nil #'string=))
         (existing (gethash msg-id claude-stream-messages)))
    
    ;; Initialize or update message accumulator
    (unless existing
      (setq existing (list :id msg-id :content nil :complete nil)))
    
    ;; Process content items
    (dolist (item content)
      (when (listp item)
        (let ((content-type (alist-get "type" item nil nil #'string=)))
          (cond
           ;; Text content
           ((string= content-type "text")
            (let ((text (alist-get "text" item nil nil #'string=)))
              (claude-stream-append-to-output proc (format "Assistant: %s\n" text))))
           
           ;; Tool use
           ((string= content-type "tool_use")
            (let ((tool-name (alist-get "name" item nil nil #'string=))
                  (tool-id (alist-get "id" item nil nil #'string=))
                  (tool-input (alist-get "input" item nil nil #'string=)))
              (message "[DEBUG] Tool use detected: %s" tool-name)
              (claude-stream-append-to-output 
               proc 
               (format "🔧 Tool Use: %s (ID: %s)\n   Input: %S\n" 
                       tool-name tool-id tool-input))))
           
           ;; Thinking content
           ((string= content-type "thinking")
            (let ((thinking (alist-get "thinking" item nil nil #'string=)))
              (claude-stream-append-to-output 
               proc 
               (format "💭 Thinking: %s\n" thinking))))))))
    
    ;; Update message in hash table
    (puthash msg-id existing claude-stream-messages)
    
    ;; If message is complete, run callback
    (when (and stop-reason claude-stream-callback)
      (funcall claude-stream-callback data))))

(defun claude-stream-handle-user (proc data)
  "Handle user message DATA from PROC."
  (let* ((message (alist-get "message" data nil nil #'string=))
         (content (alist-get "content" message nil nil #'string=)))
    (dolist (item content)
      (when (listp item)
        (let ((tool-use-id (alist-get "tool_use_id" item nil nil #'string=))
              (tool-result (alist-get "tool_result" item nil nil #'string=)))
          (when tool-result
            (let ((result-content (alist-get "content" tool-result nil nil #'string=)))
              (claude-stream-append-to-output 
               proc 
               (format "📊 Tool Result (ID: %s):\n%s\n\n" 
                       tool-use-id 
                       (claude-stream-truncate-string result-content 500))))))))))

(defun claude-stream-truncate-string (str max-length)
  "Truncate STR to MAX-LENGTH characters with ellipsis if needed."
  (if (> (length str) max-length)
      (concat (substring str 0 max-length) "...")
    str))

(defun claude-stream-append-to-output (proc text)
  "Append TEXT to the output buffer associated with PROC."
  (when-let ((output-buf (with-current-buffer (process-buffer proc)
                           claude-stream-output-buffer)))
    (with-current-buffer output-buf
      (goto-char (point-max))
      (insert text)
      (goto-char (point-max)))))

(defun claude-stream-create-process (name command output-buffer-name &optional callback)
  "Create Claude stream process NAME running COMMAND with OUTPUT-BUFFER-NAME.
Optional CALLBACK is called when complete messages are received."
  (message "[DEBUG] Starting Claude stream process")
  (message "[DEBUG] Command: %s" command)
  (let* ((proc-buffer (generate-new-buffer (format " *%s-proc*" name)))
         (output-buffer (claude-stream-create-output-buffer output-buffer-name))
         (proc (start-process-shell-command name proc-buffer command)))
    (message "[DEBUG] Process started: %s (PID: %s)" proc (process-id proc))
    
    (with-current-buffer proc-buffer
      (setq-local claude-stream-buffer "")
      (setq-local claude-stream-messages (make-hash-table :test 'equal))
      (setq-local claude-stream-output-buffer output-buffer)
      (setq-local claude-stream-callback callback))
    
    (set-process-filter proc #'claude-stream-process-filter)
    (set-process-sentinel proc #'claude-stream-sentinel)
    
    ;; Display output buffer
    (display-buffer output-buffer)
    
    proc))

(defun claude-stream-sentinel (proc event)
  "Process sentinel for PROC with EVENT."
  (unless (process-live-p proc)
    (with-current-buffer (process-buffer proc)
      (when (> (length claude-stream-buffer) 0)
        (message "Incomplete data at end: %s" claude-stream-buffer))
      (claude-stream-append-to-output proc "\n=== Stream Ended ===\n"))
    (kill-buffer (process-buffer proc))
    (message "Claude stream %s: %s" (process-name proc) event)))

;; Example usage function
(defun claude-stream-test (command)
  "Test Claude stream parsing with COMMAND."
  (interactive "sCommand: ")
  (claude-stream-create-process
   "claude-test"
   command
   "*Claude Stream Output*"
   (lambda (data)
     (message "Complete message received: %s" 
              (alist-get "id" (alist-get "message" data nil nil #'string=) 
                         nil nil #'string=)))))

(provide 'claude-stream-parser)
;;; claude-stream-parser.el ends here