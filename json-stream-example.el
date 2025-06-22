;;; json-stream-example.el --- Example async JSON stream usage -*- lexical-binding: t; -*-

(require 'json-stream-parser)

;; Example 1: Simple echo callback
(defun start-json-echo-process ()
  "Start a process that echoes JSON data."
  (interactive)
  (json-stream-create-process 
   "json-echo"
   "your-command-here"
   (lambda (data)
     (message "Received: %s" (alist-get "type" data nil nil #'string=)))))

;; Example 2: More complex callback with buffer updates
(defun start-json-stream-with-buffer ()
  "Start JSON stream and update a buffer."
  (interactive)
  (let ((output-buffer (get-buffer-create "*JSON Stream Output*")))
    (with-current-buffer output-buffer
      (erase-buffer))
    
    (json-stream-create-process
     "json-stream"
     "your-command-here"
     (lambda (data)
       (with-current-buffer output-buffer
         (goto-char (point-max))
         (insert (format "%s: %S\n" 
                        (format-time-string "%H:%M:%S")
                        data)))))))

;; Example 3: Process your specific JSON format
(defun process-assistant-stream (command)
  "Process assistant JSON stream from COMMAND."
  (json-stream-create-process
   "assistant-stream"
   command
   (lambda (data)
     (let ((type (alist-get "type" data nil nil #'string=)))
       (when (string= type "assistant")
         (let* ((msg (alist-get "message" data nil nil #'string=))
                (content (alist-get "content" msg nil nil #'string=)))
           (dolist (item content)
             (when (and (listp item)
                       (string= (alist-get "type" item nil nil #'string=) "tool_use"))
               (let ((tool-name (alist-get "name" item nil nil #'string=))
                     (tool-input (alist-get "input" item nil nil #'string=)))
                 (message "[%s] %s: %s"
                         (format-time-string "%H:%M:%S")
                         tool-name
                         (alist-get "description" tool-input nil nil #'string=)))))))))))

;; Example 4: Test with local file simulation
(defun test-with-file-stream (file)
  "Test streaming JSON from FILE."
  (interactive "fJSON file: ")
  (json-stream-create-process
   "file-stream"
   (format "tail -f %s" file)
   (lambda (data)
     (message "File stream: %S" data))))

;; Example 5: Multiple concurrent streams
(defvar my-json-processes nil
  "List of active JSON stream processes.")

(defun start-multiple-streams ()
  "Start multiple JSON streams."
  (interactive)
  (setq my-json-processes
        (list
         (json-stream-create-process
          "stream-1"
          "command1"
          (lambda (data) (message "Stream 1: %s" data)))
         
         (json-stream-create-process
          "stream-2"
          "command2"
          (lambda (data) (message "Stream 2: %s" data))))))

(defun stop-all-streams ()
  "Stop all JSON streams."
  (interactive)
  (dolist (proc my-json-processes)
    (when (process-live-p proc)
      (kill-process proc)))
  (setq my-json-processes nil))

(provide 'json-stream-example)
;;; json-stream-example.el ends here