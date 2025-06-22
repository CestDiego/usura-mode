;;; json-stream-parser.el --- Parse streaming JSON output -*- lexical-binding: t; -*-

;;; Commentary:
;; Parse newline-delimited JSON stream asynchronously

;;; Code:

(require 'json)

(defvar-local json-stream-buffer ""
  "Buffer for accumulating incomplete JSON data.")

(defvar-local json-stream-callback nil
  "Callback function for processing parsed JSON.")

(defun json-stream-process-filter (proc string)
  "Process filter for handling streaming JSON output.
PROC is the process, STRING is the incoming data."
  (with-current-buffer (process-buffer proc)
    (setq json-stream-buffer (concat json-stream-buffer string))
    
    ;; Process complete lines
    (while (string-match "\n" json-stream-buffer)
      (let ((line (substring json-stream-buffer 0 (match-beginning 0))))
        (setq json-stream-buffer (substring json-stream-buffer (match-end 0)))
        (when (> (length line) 0)
          (json-stream-process-line proc line))))))

(defun json-stream-process-line (proc line)
  "Process a single LINE of JSON data from PROC."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (json-key-type 'string)
             (data (json-read-from-string line)))
        
        ;; Call the callback if set
        (with-current-buffer (process-buffer proc)
          (when json-stream-callback
            (funcall json-stream-callback data))))
    (json-readtable-error
     (message "JSON parse error in line: %s" line))
    (error
     (message "Error processing JSON: %s" err))))

(defun json-stream-create-process (name command &optional callback)
  "Create async process NAME running COMMAND with optional CALLBACK.
CALLBACK receives parsed JSON objects."
  (let* ((buffer (generate-new-buffer (format " *%s*" name)))
         (proc (start-process-shell-command name buffer command)))
    (with-current-buffer buffer
      (setq-local json-stream-buffer "")
      (setq-local json-stream-callback (or callback #'json-stream-default-callback)))
    (set-process-filter proc #'json-stream-process-filter)
    (set-process-sentinel proc #'json-stream-sentinel)
    proc))

(defun json-stream-default-callback (data)
  "Default callback that echoes DATA."
  (message "JSON: %S" data))

(defun json-stream-sentinel (proc event)
  "Process sentinel for PROC with EVENT."
  (unless (process-live-p proc)
    (with-current-buffer (process-buffer proc)
      (when (> (length json-stream-buffer) 0)
        (message "Incomplete JSON data: %s" json-stream-buffer)))
    (kill-buffer (process-buffer proc))
    (message "Process %s: %s" (process-name proc) event)))

;; Example usage:
;; (json-stream-create-process "my-json-stream" 
;;                            "curl -N https://api.example.com/stream"
;;                            (lambda (data)
;;                              (message "Got: %s" (alist-get "type" data nil nil #'string=))))

(provide 'json-stream-parser)
;;; json-stream-parser.el ends here