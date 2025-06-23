;;; usura-mode.el --- Claude AI integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2024

;; Author: Your Name
;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: ai, claude, llm, tools
;; URL: https://github.com/yourusername/usura-mode

;;; Commentary:

;; Usura Mode provides deep integration with Claude AI in Emacs, featuring:
;;
;; - High-performance streaming JSON parser for Claude's output
;; - Beautiful text-based UI with Unicode box drawing
;; - Support for all Claude event types (text, thinking, tool use, etc.)
;; - Multiple UI components for different use cases
;; - Async processing that doesn't block Emacs
;;
;; Quick Start:
;;
;;   (require 'usura-mode)
;;   
;;   ;; Interactive commands:
;;   M-x usura-claude-stream        ; Chat with Claude using new UI
;;   M-x usura-claude-advice        ; Get advice about current buffer
;;   M-x usura-claude-test-ui       ; Test the UI with sample data
;;
;; Key Features:
;;
;; 1. Stream Processing: Handles Claude's streaming JSON output efficiently
;; 2. Rich UI: Beautiful text-based interface with proper formatting
;; 3. Tool Support: Visualizes all tool uses and results
;; 4. Performance: Batched rendering, minimal redraw, async operations
;;
;; Architecture:
;;
;; - usura-mode.el          : Main entry point and user commands
;; - claude-stream-ui.el    : High-performance UI system
;; - claude-stream-parser.el: Legacy parser (kept for compatibility)
;; - json-stream-parser.el  : General JSON stream parsing
;;
;; Customization:
;;
;;   (setq claude-stream-chunk-size 8192)    ; Larger chunks
;;   (setq claude-stream-render-delay 0.1)   ; Slower updates
;;
;; For more information, see CLAUDE-STREAM-UI-README.md

;;; Code:

;; Ensure the package directory is in load-path
(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (when (and dir (not (member dir load-path)))
    (add-to-list 'load-path dir)))

;; Core requirements
(require 'json)
(require 'cl-lib)

;; Load the main UI system
(require 'claude-stream-ui)

;; Load the org-mode UI system
(require 'claude-stream-org-ui nil t)

;; Load the project explorer
(require 'claude-project-explorer nil t)

;; Load the dynamic commands system
(require 'usura-claude-commands nil t)

;; Optional components - loaded on demand
(autoload 'claude-stream-ui-test "claude-stream-ui-test" "Test Claude Stream UI" t)
(autoload 'claude-stream-ui-demo "claude-stream-ui-test" "Demo Claude Stream UI" t)
(autoload 'claude-stream-ui-test-advice-file "claude-stream-ui-test" "Test with advice file" t)
(autoload 'claude-stream-visual-test "claude-stream-visual-test" "Visual test of UI" t)

;;; Customization

(defgroup usura nil
  "Claude AI integration for Emacs."
  :group 'tools
  :prefix "usura-")

(defcustom usura-claude-executable "claude"
  "Path to the Claude executable."
  :type 'string
  :group 'usura)

(defcustom usura-default-output-format "stream-json"
  "Default output format for Claude commands."
  :type '(choice (const "stream-json")
                 (const "json")
                 (const "text"))
  :group 'usura)

(defcustom usura-verbose t
  "Whether to use verbose output."
  :type 'boolean
  :group 'usura)

(defcustom usura-use-org-mode t
  "Whether to use org-mode formatting for Claude responses."
  :type 'boolean
  :group 'usura)

;;; Core Functions

(defun usura-build-claude-command (prompt &optional flags)
  "Build a Claude command with PROMPT and optional FLAGS."
  (format "claude -p \"%s\" --output-format stream-json --verbose"
          (shell-quote-argument prompt)))

;;; User Commands

;;;###autoload
(defun usura-claude-stream (prompt)
  "Start a Claude session with PROMPT using the streaming UI."
  (interactive "sClaude prompt: ")
  (let ((command (usura-build-claude-command prompt)))
    (if (and usura-use-org-mode (featurep 'claude-stream-org-ui))
        (claude-stream-org-ui-create-process
         (format "claude-%s" (format-time-string "%H%M%S"))
         command)
      (claude-stream-ui-create-process
       (format "claude-%s" (format-time-string "%H%M%S"))
       command))))

;; The advice command is now handled by usura-claude-commands.el
;; This wrapper ensures backward compatibility
;;;###autoload
(defun usura-claude-advice ()
  "Get Claude's advice about the current buffer.
(Compatibility wrapper - actual implementation may be in usura-claude-commands)"
  (interactive)
  ;; Try to use the dynamic command if available
  (if (and (featurep 'usura-claude-commands)
           (fboundp 'usura-claude-advice))
      (call-interactively 'usura-claude-advice)
    ;; Fallback implementation
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (truncated (if (> (length content) 4000)
                          (concat (substring content 0 4000) "\n[... truncated ...]")
                        content))
           (prompt (format "/advice about this content:\n\n%s" truncated))
           (command (usura-build-claude-command prompt)))
      (if (and usura-use-org-mode (featurep 'claude-stream-org-ui))
          (claude-stream-org-ui-create-process "claude-advice" command)
        (claude-stream-ui-create-process "claude-advice" command)))))

;;;###autoload
(defun usura-claude-region (start end)
  "Send the region from START to END to Claude."
  (interactive "r")
  (let* ((content (buffer-substring-no-properties start end))
         (prompt (read-string "Claude prompt for region: " "Analyze this: "))
         (full-prompt (format "%s\n\n%s" prompt content))
         (command (usura-build-claude-command full-prompt)))
    (if (and usura-use-org-mode (featurep 'claude-stream-org-ui))
        (claude-stream-org-ui-create-process "claude-region" command)
      (claude-stream-ui-create-process "claude-region" command))))

;;;###autoload
(defun usura-claude-insert-response ()
  "Ask Claude a question and insert the response at point."
  (interactive)
  (let* ((prompt (read-string "Claude prompt: "))
         (buffer (current-buffer))
         (point (point))
         (command (usura-build-claude-command prompt)))
    (claude-stream-ui-create-process
     "claude-insert"
     command
     (lambda (data)
       ;; Insert text responses back into the original buffer
       (when (string= (alist-get "type" data nil nil #'string=) "assistant")
         (let ((message (alist-get "message" data nil nil #'string=)))
           (when message
             (let ((content (alist-get "content" message nil nil #'string=)))
               (dolist (item content)
                 (when (and (listp item)
                            (string= (alist-get "type" item nil nil #'string=) "text"))
                   (let ((text (alist-get "text" item nil nil #'string=)))
                     (with-current-buffer buffer
                       (save-excursion
                         (goto-char point)
                         (insert text)
                         (setq point (point)))))))))))))))

;;;###autoload
(defun usura-claude-test-ui ()
  "Test the Claude UI with sample data."
  (interactive)
  (require 'claude-stream-ui-test)
  (claude-stream-ui-demo))

;;;###autoload
(defun usura-claude-test-advice-file ()
  "Test UI by parsing the advice file."
  (interactive)
  (require 'claude-stream-ui-test)
  (claude-stream-ui-test-advice-file))

;;; Key Bindings

(defvar usura-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c u s") #'usura-claude-stream)
    (define-key map (kbd "C-c u a") #'usura-claude-advice)
    (define-key map (kbd "C-c u r") #'usura-claude-region)
    (define-key map (kbd "C-c u i") #'usura-claude-insert-response)
    (define-key map (kbd "C-c u t") #'usura-claude-test-ui)
    (define-key map (kbd "C-c u o") #'usura-claude-stream-org)
    (define-key map (kbd "C-c u p") #'claude-project-explorer)
    (define-key map (kbd "C-c u f") #'claude-project-open-jsonl-file)
    ;; Reserve C-c u c for dynamic commands
    (define-key map (kbd "C-c u C") #'usura-claude-command)
    (define-key map (kbd "C-c u L") #'usura-claude-commands-list)
    map)
  "Keymap for Usura mode commands.")

;;;###autoload
(define-minor-mode usura-mode
  "Minor mode for Claude AI integration.
\\{usura-mode-map}"
  :lighter " Usura"
  :keymap usura-mode-map
  :group 'usura)

;;;###autoload
(define-globalized-minor-mode global-usura-mode usura-mode
  (lambda () (usura-mode 1))
  :group 'usura)

;;; Integration with Org Mode

(defun usura-org-claude-block ()
  "Create an Org source block with Claude's response."
  (interactive)
  (let ((prompt (read-string "Claude prompt: ")))
    (insert (format "#+BEGIN_SRC claude :prompt \"%s\"\n" prompt))
    (insert "# Waiting for Claude...\n")
    (insert "#+END_SRC\n")
    (forward-line -2)
    (let ((block-start (point)))
      (usura-claude-stream prompt))))

;;; Utility Functions

(defun usura-list-sessions ()
  "List all active Claude sessions."
  (interactive)
  (let ((sessions '()))
    (dolist (buffer (buffer-list))
      (when (string-prefix-p "*Claude:" (buffer-name buffer))
        (push (buffer-name buffer) sessions)))
    (if sessions
        (message "Active Claude sessions: %s" (string-join sessions ", "))
      (message "No active Claude sessions"))))

(defun usura-kill-all-sessions ()
  "Kill all Claude session buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-prefix-p "*Claude:" (buffer-name buffer))
      (kill-buffer buffer)))
  (message "All Claude sessions terminated"))

;;; Legacy Support

;; Keep the original function for backward compatibility
(defun usura/advice ()
  "Legacy advice function - now uses new UI."
  (interactive)
  (call-interactively #'usura-claude-advice))

;; Re-export important functions from claude-stream-ui
(defalias 'usura-create-stream-process #'claude-stream-ui-create-process)
(defalias 'usura-stream-test #'claude-stream-ui-test)

;;; Package Components Export

;; Make all sub-packages available when usura-mode is loaded
;; Users can then (require 'usura-mode) and access everything

;; Export all sub-packages so they can be required individually if needed
(with-eval-after-load 'usura-mode
  ;; Core components
  (require 'claude-stream-ui nil t)
  (require 'claude-stream-org-ui nil t)
  (require 'json-stream-parser nil t)
  (require 'claude-stream-parser nil t))

;; Provide access to test functions
(defun usura-load-tests ()
  "Load all test modules for Usura mode."
  (interactive)
  (require 'claude-stream-ui-test)
  (require 'claude-stream-visual-test)
  (require 'claude-stream-ui-ert-tests)
  (message "Usura test modules loaded"))

;; Convenience function to load everything
(defun usura-load-all ()
  "Load all Usura mode components."
  (interactive)
  (require 'claude-stream-ui)
  (require 'claude-stream-org-ui nil t)
  (require 'claude-project-explorer nil t)
  (require 'usura-claude-commands nil t)
  (require 'claude-stream-parser)
  (require 'json-stream-parser)
  (require 'claude-stream-ui-test nil t)
  (require 'claude-stream-visual-test nil t)
  ;; Initialize commands if available
  (when (fboundp 'usura-claude-commands-initialize)
    (usura-claude-commands-initialize))
  (message "All Usura components loaded"))

;;;###autoload
(defun usura-claude-stream-org (prompt)
  "Start a Claude session with PROMPT using org-mode UI."
  (interactive "sClaude prompt: ")
  (let ((command (usura-build-claude-command prompt)))
    (if (featurep 'claude-stream-org-ui)
        (claude-stream-org-ui-create-process
         (format "claude-org-%s" (format-time-string "%H%M%S"))
         command)
      (error "Org-mode UI not available"))))

;;; Public API for package users

;; Users can access these after (require 'usura-mode)
(defvaralias 'usura-ui-templates 'claude-stream-ui-templates
  "Access to UI templates for customization.")

(defvaralias 'usura-chunk-size 'claude-stream-chunk-size
  "Buffer chunk size for stream processing.")

(defvaralias 'usura-render-delay 'claude-stream-render-delay
  "Delay between render updates.")

;; Function aliases for cleaner API
(defalias 'usura-parse-json-line #'claude-stream--parse-json-line
  "Parse a JSON line from Claude stream.")

(defalias 'usura-render-event #'claude-stream--render-event
  "Render a Claude event to the buffer.")

;;; Package Export

(provide 'usura-mode)

;; Export sub-features that can be required separately
(provide 'usura-mode-ui)     ; When user wants just the UI
(provide 'usura-mode-parser) ; When user wants just parsing

;;; usura-mode.el ends here
