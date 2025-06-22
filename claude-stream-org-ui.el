;;; claude-stream-org-ui.el --- Org-mode based Claude stream UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Org-mode based stream processor for Claude responses
;; Uses hierarchical org-mode structure with syntax highlighting

;;; Code:

(require 'json)
(require 'subr-x)
(require 'cl-lib)
(require 'org)

;; Performance optimizations
(defcustom claude-stream-org-chunk-size 4096
  "Buffer chunk size for stream processing."
  :type 'integer
  :group 'claude-stream)

(defcustom claude-stream-org-render-delay 0.05
  "Delay in seconds between render updates to prevent flicker."
  :type 'float
  :group 'claude-stream)

;; Stream state management
(defvar-local claude-stream-org--buffer "")
(defvar-local claude-stream-org--messages (make-hash-table :test 'equal))
(defvar-local claude-stream-org--active-tools (make-hash-table :test 'equal))
(defvar-local claude-stream-org--render-timer nil)
(defvar-local claude-stream-org--render-queue nil)
(defvar-local claude-stream-org--current-heading-level 1)
(defvar-local claude-stream-org--in-src-block nil)

;; JSON parser (reuse from original)
(defun claude-stream-org--parse-json-line (line)
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
(defun claude-stream-org--process-filter (proc string)
  "High-performance filter for PROC receiving STRING."
  (with-current-buffer (process-buffer proc)
    (setq claude-stream-org--buffer (concat claude-stream-org--buffer string))
    
    ;; Process complete lines
    (let ((lines nil))
      (while (string-match "\n" claude-stream-org--buffer)
        (push (substring claude-stream-org--buffer 0 (match-beginning 0)) lines)
        (setq claude-stream-org--buffer (substring claude-stream-org--buffer (match-end 0))))
      
      ;; Batch process lines
      (when lines
        (claude-stream-org--batch-process-lines (nreverse lines)))))))

(defun claude-stream-org--batch-process-lines (lines)
  "Process LINES in batch for efficiency."
  (dolist (line lines)
    (when (> (length line) 0)
      (when-let ((data (claude-stream-org--parse-json-line line)))
        (claude-stream-org--queue-render data)))))

;; Render queue for batched updates
(defun claude-stream-org--queue-render (data)
  "Queue DATA for rendering."
  (push data claude-stream-org--render-queue)
  (unless claude-stream-org--render-timer
    (setq claude-stream-org--render-timer
          (run-with-timer claude-stream-org-render-delay nil
                          #'claude-stream-org--flush-render-queue))))

(defun claude-stream-org--flush-render-queue ()
  "Flush all queued renders at once."
  (setq claude-stream-org--render-timer nil)
  (let ((inhibit-redisplay t)
        (queue (nreverse claude-stream-org--render-queue)))
    (setq claude-stream-org--render-queue nil)
    (save-excursion
      (dolist (data queue)
        (claude-stream-org--render-event data))))
  (redisplay))

;; Org-mode specific rendering helpers
(defun claude-stream-org--insert-heading (level title &optional tags)
  "Insert org heading at LEVEL with TITLE and optional TAGS."
  (goto-char (point-max))
  (insert (make-string level ?*) " " title)
  (when tags
    (insert " :" (mapconcat #'identity tags ":") ":"))
  (insert "\n"))

(defun claude-stream-org--ensure-not-in-src-block ()
  "Ensure we're not in a src block, close it if we are."
  (when claude-stream-org--in-src-block
    (goto-char (point-max))
    (insert "#+end_src\n")
    (setq claude-stream-org--in-src-block nil)))

;; Event rendering with org-mode structure
(defun claude-stream-org--render-event (data)
  "Render DATA event using org-mode structure."
  (let ((type (alist-get "type" data nil nil #'string=)))
    (cond
     ((string= type "system")
      (claude-stream-org--render-system data))
     ((string= type "assistant")
      (claude-stream-org--render-assistant data))
     ((string= type "user")
      (claude-stream-org--render-user data))
     ((string= type "result")
      (claude-stream-org--render-result data)))))

(defun claude-stream-org--render-system (data)
  "Render system DATA."
  (let ((subtype (alist-get "subtype" data nil nil #'string=)))
    (when (string= subtype "init")
      (claude-stream-org--ensure-not-in-src-block)
      (claude-stream-org--insert-heading 1 "Claude Session" '("session"))
      (goto-char (point-max))
      (insert ":PROPERTIES:\n")
      (insert (format ":SESSION_ID: %s\n" (substring (alist-get "session_id" data nil nil #'string=) 0 8)))
      (insert (format ":MODEL: %s\n" (alist-get "model" data nil nil #'string=)))
      (insert (format ":MODE: %s\n" (alist-get "permissionMode" data nil nil #'string=)))
      (insert (format ":TOOLS_COUNT: %d\n" (length (alist-get "tools" data nil nil #'string=))))
      (insert ":END:\n\n"))))

(defun claude-stream-org--render-assistant (data)
  "Render assistant DATA."
  (let* ((message (alist-get "message" data nil nil #'string=))
         (msg-id (alist-get "id" message nil nil #'string=))
         (content (alist-get "content" message nil nil #'string=))
         (stop-reason (alist-get "stop_reason" message nil nil #'string=))
         (existing (gethash msg-id claude-stream-org--messages)))
    
    ;; First message with this ID
    (unless existing
      (claude-stream-org--ensure-not-in-src-block)
      (claude-stream-org--insert-heading 2 "Assistant Response" 
                                       (if stop-reason '("complete") '("streaming")))
      (goto-char (point-max))
      (insert ":PROPERTIES:\n")
      (insert (format ":MESSAGE_ID: %s\n" msg-id))
      (insert ":END:\n\n")
      (puthash msg-id (point-marker) claude-stream-org--messages))
    
    ;; Render content items
    (dolist (item content)
      (when (listp item)
        (claude-stream-org--render-content-item item msg-id)))))

(defun claude-stream-org--render-content-item (item msg-id)
  "Render content ITEM for MSG-ID."
  (let ((content-type (alist-get "type" item nil nil #'string=)))
    (cond
     ;; Text content - wrap in markdown src block
     ((string= content-type "text")
      (let ((text (alist-get "text" item nil nil #'string=)))
        (when (> (length text) 0)
          (unless claude-stream-org--in-src-block
            (claude-stream-org--ensure-not-in-src-block)
            (goto-char (point-max))
            (insert "#+begin_src markdown\n")
            (setq claude-stream-org--in-src-block t))
          (goto-char (point-max))
          (insert text))))
     
     ;; Thinking content
     ((string= content-type "thinking")
      (claude-stream-org--ensure-not-in-src-block)
      (let ((thinking (alist-get "thinking" item nil nil #'string=)))
        (claude-stream-org--insert-heading 3 "Thinking" '("thinking"))
        (goto-char (point-max))
        (insert "#+begin_quote\n")
        (insert thinking)
        (insert "\n#+end_quote\n")))
     
     ;; Tool use
     ((string= content-type "tool_use")
      (claude-stream-org--ensure-not-in-src-block)
      (let ((tool-id (alist-get "id" item nil nil #'string=))
            (tool-name (alist-get "name" item nil nil #'string=))
            (tool-input (alist-get "input" item nil nil #'string=)))
        ;; Track active tool
        (puthash tool-id (list :name tool-name :start (current-time)) 
                 claude-stream-org--active-tools)
        ;; Render as org heading
        (claude-stream-org--insert-heading 3 (format "Tool: %s" tool-name) '("tool"))
        (goto-char (point-max))
        (insert ":PROPERTIES:\n")
        (insert (format ":TOOL_ID: %s\n" (substring tool-id -8)))
        (insert ":END:\n\n")
        ;; Render inputs as properties or list
        (when (listp tool-input)
          (insert "Parameters:\n")
          (dolist (field tool-input)
            (when (consp field)
              (insert (format "- ~%s~: %s\n" 
                              (car field)
                              (claude-stream-org--format-value (cdr field))))))))))))

(defun claude-stream-org--render-user (data)
  "Render user DATA (tool results)."
  (let* ((message (alist-get "message" data nil nil #'string=))
         (content (alist-get "content" message nil nil #'string=)))
    (dolist (item content)
      (when (listp item)
        (let ((tool-use-id (alist-get "tool_use_id" item nil nil #'string=))
              (tool-result (alist-get "tool_result" item nil nil #'string=))
              (is-error (alist-get "is_error" item)))
          (when tool-result
            (claude-stream-org--ensure-not-in-src-block)
            (let* ((tool-info (gethash tool-use-id claude-stream-org--active-tools))
                   (tool-name (plist-get tool-info :name))
                   (content-text (alist-get "content" tool-result nil nil #'string=)))
              (claude-stream-org--insert-heading 
               4 
               (format "Result: %s" (or tool-name (substring tool-use-id -8)))
               (if is-error '("error" "result") '("success" "result")))
              (goto-char (point-max))
              (insert "#+begin_example\n")
              (insert content-text)
              (insert "\n#+end_example\n")
              ;; Clear active tool
              (remhash tool-use-id claude-stream-org--active-tools))))))))

(defun claude-stream-org--render-result (data)
  "Render final result DATA."
  (claude-stream-org--ensure-not-in-src-block)
  (claude-stream-org--insert-heading 2 "Session Summary" '("summary"))
  (goto-char (point-max))
  (let ((duration (alist-get "duration_ms" data))
        (api-duration (alist-get "duration_api_ms" data))
        (turns (alist-get "num_turns" data))
        (cost (alist-get "total_cost_usd" data))
        (usage (alist-get "usage" data)))
    (insert "| Metric | Value |\n")
    (insert "|--------+-------|\n")
    (insert (format "| Duration | %s ms (API: %s ms) |\n" duration api-duration))
    (insert (format "| Turns | %d |\n" turns))
    (insert (format "| Cost | $%.6f |\n" cost))
    (insert (format "| Input Tokens | %d |\n" (alist-get "input_tokens" usage)))
    (insert (format "| Output Tokens | %d |\n" (alist-get "output_tokens" usage)))
    (insert (format "| Cache Created | %d |\n" (alist-get "cache_creation_input_tokens" usage)))
    (insert (format "| Cache Read | %d |\n" (alist-get "cache_read_input_tokens" usage)))
    (insert "\n")
    ;; Add the actual result text
    (claude-stream-org--insert-heading 3 "Final Output" '("output"))
    (goto-char (point-max))
    (insert "#+begin_src markdown\n")
    (insert (alist-get "result" data nil nil #'string=))
    (insert "\n#+end_src\n")))

;; Helper functions
(defun claude-stream-org--format-value (value)
  "Format VALUE for display in org-mode."
  (let ((str (format "%S" value)))
    (if (> (length str) 80)
        (concat (substring str 0 77) "...")
      str)))

;; Main entry point
(defun claude-stream-org-ui-create-process (name command &optional callback)
  "Create org-mode based Claude stream process NAME with COMMAND.
Optional CALLBACK is called on complete messages."
  (let* ((buffer (generate-new-buffer (format "*Claude-Org: %s*" name)))
         (proc (start-process-shell-command name buffer command)))
    
    (with-current-buffer buffer
      ;; Enable org-mode
      (org-mode)
      
      ;; Initialize buffer-local state
      (setq claude-stream-org--buffer ""
            claude-stream-org--messages (make-hash-table :test 'equal)
            claude-stream-org--active-tools (make-hash-table :test 'equal)
            claude-stream-org--render-timer nil
            claude-stream-org--render-queue nil
            claude-stream-org--current-heading-level 1
            claude-stream-org--in-src-block nil)
      
      ;; Set up org-mode preferences
      (setq-local org-hide-leading-stars t)
      (setq-local org-startup-indented t)
      (setq-local org-src-fontify-natively t)
      (setq-local org-fontify-quote-and-verse-blocks t)
      (setq-local org-startup-folded nil)
      
      (goto-char (point-min))
      (insert "#+TITLE: Claude Session - " name "\n")
      (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n")
      (insert "#+OPTIONS: toc:nil\n\n"))
    
    (set-process-filter proc #'claude-stream-org--process-filter)
    (set-process-sentinel proc
                          (lambda (p e)
                            (with-current-buffer (process-buffer p)
                              (claude-stream-org--ensure-not-in-src-block)
                              (goto-char (point-max))
                              (insert "\n* Process Status :meta:\n")
                              (insert "Stream ended: " e "\n"))))
    
    ;; Display buffer with nice window settings
    (let ((window (display-buffer buffer 
                                  '(display-buffer-pop-up-window
                                    (window-height . 0.5)))))
      (set-window-dedicated-p window t))
    
    proc))

;; Debug helper
(defun claude-stream-org--debug-log (msg &rest args)
  "Log debug MSG with ARGS to *Claude Debug* buffer."
  (with-current-buffer (get-buffer-create "*Claude Debug*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] ")
            (apply #'format msg args)
            "\n")))

;; Interactive commands
(defun claude-stream-org-ui-test ()
  "Test the Claude org-mode stream UI."
  (interactive)
  (let ((prompt (read-string "Claude prompt: " "/advice ")))
    (claude-stream-org-ui-create-process
     "test"
     (format "claude -p \"%s\" --output-format stream-json --verbose"
             prompt))))

(provide 'claude-stream-org-ui)
;;; claude-stream-org-ui.el ends here