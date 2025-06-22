;;; claude-stream-ui-ert-tests.el --- ERT tests for Claude Stream UI -*- lexical-binding: t; -*-

;;; Commentary:
;; Automated tests for Claude Stream UI using ERT (Emacs Regression Testing)
;; Run from command line: emacs -batch -l ert -l claude-stream-ui.el -l claude-stream-ui-ert-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'claude-stream-ui)

;; Test data from the visible conversation output
(defconst claude-test-events
  '(;; System init from output
    "{\"type\":\"system\",\"subtype\":\"init\",\"cwd\":\"/Users/wayrapro/Documents/Projects/MCP/google-flights-mcp\",\"session_id\":\"8113cd23-f26a-4a12-bd19-a7c24d738130\",\"tools\":[\"Task\",\"Bash\",\"Glob\",\"Grep\",\"LS\",\"exit_plan_mode\",\"Read\",\"Edit\",\"MultiEdit\",\"Write\",\"NotebookRead\",\"NotebookEdit\",\"WebFetch\",\"TodoRead\",\"TodoWrite\",\"WebSearch\"],\"mcp_servers\":[],\"model\":\"claude-opus-4-20250514\",\"permissionMode\":\"default\",\"apiKeySource\":\"none\"}"
    
    ;; Assistant with thinking
    "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01ECGbigJq6ktvfj7YogUFLT\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"thinking\",\"thinking\":\"The user is asking for advice about whether to open source their project. They work at Google and need to clear it with them, but they're torn because they think the project could be valuable to the developer community. The project appears to be about supporting multiple LLM models, possibly as a proxy or sidecar service. They also want me to research whether similar solutions already exist.\",\"signature\":\"Eu8GCkYIBBgCKkAaQXCkglJtvVLY8ZcYva8jalCYgOaAj8mrOPIpms180zWa9IuwtOqILg++5C7FrK83NlANUHlw0voSb8r1ZW5EEgxaipP8YLWz9QMJI+waDPoEf0s3NPmM1O6eOyIwIODAvHOuv47m2ka6Bq7oChf9/JFXn6SkDrzoC4uOSEXtfize5xI/TCXZ1j8lDQf1KtYFx1VCcPu2I5tLbFUX+MOeBsa9eQnRx7ayRpH9kcYSfZ7d5s1TQIxCZ3pPyHdlSzRUYkHM3Gz6RvsFwva0c6o0YNhgjySgNcaek8qHAqP4g4jQJV14Pp0TzWPTX2MMCpyq3zbJoIbv5YUBkTk8I5tuA7N+rpr2kZtSkIXOgY06itoQf5V0spGoIcLTN8lAs5z4mCCkv66uEG9Vc1Syu27CDt5zf1r4S1AYJATMXFLPqgi6nYtCOcZyGTslj0T6dP3SfWzhLohTNMvw/xv4uCLQZY0iUZd4lHoQ+L0iPriopoxn2Y+E3LKdRMdsAKBjNMEyIoA+sL3XXsjAMVd1wJA13ua62ZP++RXVSFbnPOLta/+8xi6+t+Rz3wxWZrm6rvDl3t1KmpY0Xk08iOKorhlvl1eDJnIZY6derxg3sIBjUVZL+6M3u+wrd4Jsi5DHUU+dunSRNAUjVSRbbvlKQkgdcM0aTUXhiahUSIvyx8Mfy6BpdB+KU8K4bwAKkYXhwYfLS6utHgDjPyeKo7xOyE9U1ZV7RxxDlYmA0S51k0es0sC10+GjJCFd1QpGvCs3DglF/GO9HMiOlbeXTkpqAN9dDY+W0UxoIFdumlbKwZ6GGp7x+ggNsTCnDMFQe2+mpwV8p5rkOXXo8EeQLxkrpxqENxDXpWkOmhNPoyRFQYNlQHvGuouwJEAkgVWR6kKjeXtFfxYg1CwOcxSoSs8rRTMiCZaWxXJSIb71PF73y1LBm606hF7oJdJiTWILpek9YLWn13Lfr2IzsSSzD34oBE9DVEtRo0Ihs5UcyZSmLvP3zVudEu8FpN5IGGT7o7cGWGxJTjSQc6BTrWIOKB+4zaVsW90LvwOGfQ54xHrvFC3zEK8ZPqqd6RyaNs56Pu9u2l4PNi9IWSPOnkR6s2+V+6CuhWLaRFj/Jq8np4QlGN5TYdlxk9y9G7s3879EnoJ9KlNr3RlLhDTKGAE=\"}],\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":10,\"cache_creation_input_tokens\":5684,\"cache_read_input_tokens\":10013,\"output_tokens\":6,\"service_tier\":\"standard\"}},\"parent_tool_use_id\":null,\"session_id\":\"8113cd23-f26a-4a12-bd19-a7c24d738130\"}"
    
    ;; Tool use from output
    "{\"type\":\"assistant\",\"message\":{\"id\":\"msg_01ECGbigJq6ktvfj7YogUFLT\",\"type\":\"message\",\"role\":\"assistant\",\"model\":\"claude-opus-4-20250514\",\"content\":[{\"type\":\"tool_use\",\"id\":\"toolu_016rZp31JErD9Ak1eQPiAv7P\",\"name\":\"Task\",\"input\":{\"description\":\"Manager part perspective\",\"prompt\":\"As the Manager part in Internal Family Systems, analyze this situation\"}}],\"stop_reason\":null,\"stop_sequence\":null,\"usage\":{\"input_tokens\":10,\"cache_creation_input_tokens\":5684,\"cache_read_input_tokens\":10013,\"output_tokens\":6,\"service_tier\":\"standard\"}},\"parent_tool_use_id\":null,\"session_id\":\"8113cd23-f26a-4a12-bd19-a7c24d738130\"}"
    
    ;; Tool result from output
    "{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"tool_use_id\":\"toolu_01BS7WSjKnULzeqJd5DrDR3x\",\"type\":\"tool_result\",\"content\":\"     1→# Google Flights MCP\\n     2→\\n     3→This MCP server provides tools to interact with Google Flights data using the bundled `fast_flights` library.\"}]},\"parent_tool_use_id\":\"toolu_011MdX4tKYaqJC7L8huwuSMj\",\"session_id\":\"8113cd23-f26a-4a12-bd19-a7c24d738130\"}"))

;; Test JSON parsing
(ert-deftest claude-stream-test-json-parsing ()
  "Test JSON line parsing."
  (dolist (line claude-test-events)
    (let ((parsed (claude-stream--parse-json-line line)))
      (should (listp parsed))
      (should (stringp (alist-get "type" parsed nil nil #'string=))))))

;; Test event type detection
(ert-deftest claude-stream-test-event-types ()
  "Test that all event types are properly detected."
  (let ((event-types '()))
    (dolist (line claude-test-events)
      (let* ((data (claude-stream--parse-json-line line))
             (type (alist-get "type" data nil nil #'string=)))
        (push type event-types)))
    (should (member "system" event-types))
    (should (member "assistant" event-types))
    (should (member "user" event-types))))

;; Test rendering without errors
(ert-deftest claude-stream-test-rendering ()
  "Test that rendering doesn't throw errors."
  (with-temp-buffer
    (setq claude-stream--buffer ""
          claude-stream--messages (make-hash-table :test 'equal)
          claude-stream--active-tools (make-hash-table :test 'equal)
          claude-stream--render-queue nil)
    
    (dolist (line claude-test-events)
      (let ((data (claude-stream--parse-json-line line)))
        (should-not (condition-case err
                        (progn
                          (claude-stream--render-event data)
                          nil)
                      (error err)))))))

;; Test content type extraction
(ert-deftest claude-stream-test-content-types ()
  "Test extraction of different content types."
  (let ((thinking-found nil)
        (tool-use-found nil)
        (text-found nil))
    
    (dolist (line claude-test-events)
      (let ((data (claude-stream--parse-json-line line)))
        (when (string= (alist-get "type" data nil nil #'string=) "assistant")
          (let ((content (alist-get "content" 
                                    (alist-get "message" data nil nil #'string=) 
                                    nil nil #'string=)))
            (dolist (item content)
              (when (listp item)
                (let ((content-type (alist-get "type" item nil nil #'string=)))
                  (cond
                   ((string= content-type "thinking") (setq thinking-found t))
                   ((string= content-type "tool_use") (setq tool-use-found t))
                   ((string= content-type "text") (setq text-found t))))))))))
    
    (should thinking-found)
    (should tool-use-found)))

;; Test buffer management
(ert-deftest claude-stream-test-buffer-accumulation ()
  "Test stream buffer accumulation and line processing."
  (let ((claude-stream--buffer ""))
    ;; Test partial line accumulation
    (setq claude-stream--buffer "{\"type\":")
    (should (string= claude-stream--buffer "{\"type\":"))
    
    ;; Test complete line extraction
    (setq claude-stream--buffer "{\"type\":\"test\"}\n{\"type\":")
    (let ((lines '()))
      (while (string-match "\n" claude-stream--buffer)
        (push (substring claude-stream--buffer 0 (match-beginning 0)) lines)
        (setq claude-stream--buffer (substring claude-stream--buffer (match-end 0))))
      (should (= (length lines) 1))
      (should (string= (car lines) "{\"type\":\"test\"}")))))

;; Test template formatting
(ert-deftest claude-stream-test-templates ()
  "Test UI template formatting."
  (let ((session-template (alist-get 'session-init claude-stream-ui-templates)))
    (should (stringp session-template))
    (should (string-match "Session:" session-template))
    (should (string-match "Model:" session-template))))

;; Test helper functions
(ert-deftest claude-stream-test-helpers ()
  "Test helper functions."
  ;; Test text wrapping
  (should (string= (claude-stream--wrap-text "short" 10) "short"))
  (should (string= (claude-stream--wrap-text "very long text" 10) "very long ..."))
  
  ;; Test value formatting
  (should (string= (claude-stream--format-value "test" 10) "\"test\""))
  (should (string-match "\\.\\.\\.$" 
                        (claude-stream--format-value 
                         "very very very long value" 10))))

;; Test performance with batch processing
(ert-deftest claude-stream-test-batch-performance ()
  "Test batch processing performance."
  (let ((start-time (current-time))
        (test-lines (make-list 100 "{\"type\":\"test\",\"data\":\"line\"}")))
    (with-temp-buffer
      (setq claude-stream--render-queue nil)
      (claude-stream--batch-process-lines test-lines)
      (should (<= (length claude-stream--render-queue) 100)))
    
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (should (< elapsed 1.0))))) ; Should process 100 lines in under 1 second

;; Command line test runner
(defun claude-stream-run-tests ()
  "Run all Claude Stream UI tests."
  (interactive)
  (ert-run-tests-interactively "claude-stream-test-"))

;; Batch mode test runner for CI/command line
(defun claude-stream-run-tests-batch ()
  "Run tests in batch mode suitable for command line."
  (ert-run-tests-batch "claude-stream-test-"))

(provide 'claude-stream-ui-ert-tests)
;;; claude-stream-ui-ert-tests.el ends here