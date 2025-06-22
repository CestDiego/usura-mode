;;; test-usura-mode.el --- Test script for usura-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Test script to verify usura-mode loads correctly and can process the advice file

;;; Code:

;; Add current directory to load-path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Test 1: Basic loading
(message "=== Test 1: Loading usura-mode ===")
(condition-case err
    (progn
      (require 'usura-mode)
      (message "✓ usura-mode loaded successfully"))
  (error
   (message "✗ Failed to load usura-mode: %s" err)))

;; Test 2: Check if functions are defined
(message "\n=== Test 2: Checking functions ===")
(dolist (func '(usura-claude-stream
                usura-claude-advice
                usura-claude-region
                usura-build-claude-command))
  (if (fboundp func)
      (message "✓ %s is defined" func)
    (message "✗ %s is NOT defined" func)))

;; Test 3: Load components
(message "\n=== Test 3: Loading components ===")
(condition-case err
    (progn
      (usura-load-all)
      (message "✓ All components loaded"))
  (error
   (message "✗ Failed to load components: %s" err)))

;; Test 4: Process advice file if it exists
(message "\n=== Test 4: Testing with advice file ===")
(let ((advice-file "/Users/wayrapro/.config/doom/lisp/usura-mode/advice"))
  (if (file-exists-p advice-file)
      (condition-case err
          (progn
            (require 'claude-stream-ui-test)
            (claude-stream-ui-test-advice-file)
            (message "✓ Advice file processed successfully"))
        (error
         (message "✗ Failed to process advice file: %s" err)))
    (message "⚠ Advice file not found at %s" advice-file)))

;; Test 5: Check parentheses balance
(message "\n=== Test 5: Checking parentheses ===")
(dolist (file '("usura-mode.el" "claude-stream-ui.el" "json-stream-parser.el"))
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (condition-case err
          (progn
            (check-parens)
            (message "✓ %s has balanced parentheses" file))
        (error
         (message "✗ %s has unbalanced parentheses: %s" file err))))))

;; Test 6: Create a simple stream
(message "\n=== Test 6: Creating test stream ===")
(condition-case err
    (let ((command "echo '{\"type\":\"system\",\"subtype\":\"init\"}'"))
      (usura-create-stream-process "test" command)
      (message "✓ Stream process created"))
  (error
   (message "✗ Failed to create stream: %s" err)))

(message "\n=== All tests complete ===")

;; Run this test with:
;; emacs -batch -l test-usura-mode.el

(provide 'test-usura-mode)
;;; test-usura-mode.el ends here