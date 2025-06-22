;;; usura-mode-example.el --- Examples of using Usura mode -*- lexical-binding: t; -*-

;;; Commentary:
;; This file demonstrates various ways to use and import Usura mode components

;;; Code:

;; Method 1: Load everything through main package
(require 'usura-mode)

;; Now you can use all exported functions
(usura-claude-stream "Hello Claude!")

;; Method 2: Load specific components
(require 'claude-stream-ui)      ; Just the UI
(require 'json-stream-parser)    ; Just JSON parsing

;; Method 3: Load on demand
(usura-load-all)                 ; Load everything
(usura-load-tests)               ; Load test modules

;; Access UI templates for customization
(setq my-custom-template
      (alist-get 'session-init usura-ui-templates))

;; Configure performance settings
(setq usura-chunk-size 8192)
(setq usura-render-delay 0.1)

;; Use the API functions
(defun my-claude-handler (prompt)
  "Custom Claude handler using Usura API."
  (let ((command (usura-build-claude-command prompt)))
    (usura-create-stream-process
     "my-custom-session"
     command
     (lambda (data)
       ;; Custom processing
       (when (string= (alist-get "type" data nil nil #'string=) "assistant")
         (message "Claude says something!"))))))

;; Parse JSON lines directly
(let ((json-line "{\"type\":\"system\",\"subtype\":\"init\"}"))
  (usura-parse-json-line json-line))

;; Custom UI rendering
(with-temp-buffer
  (let ((data '((type . "system") (subtype . "init"))))
    (usura-render-event data)))

;; Integration example with org-mode
(defun my-org-claude-babel-execute (body params)
  "Execute Claude prompt in org-babel."
  (let ((prompt (or (alist-get :prompt params) body)))
    (usura-claude-stream prompt)
    ;; Return placeholder
    "See *Claude:* buffer for results"))

;; Register with org-babel
(with-eval-after-load 'ob
  (add-to-list 'org-babel-load-languages '(claude . t))
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages))

;;; Testing examples

;; Run visual tests
(when (require 'claude-stream-visual-test nil t)
  (claude-stream-visual-test))

;; Run unit tests
(when (require 'claude-stream-ui-ert-tests nil t)
  (ert-run-tests-interactively "claude-stream-test-"))

;;; Org-mode UI examples

;; Enable org-mode UI
(setq usura-use-org-mode t)

;; Use org-mode UI for a query
(usura-claude-stream "Explain recursion with examples")

;; Use specific org-mode command
(usura-claude-stream-org "Write documentation for a REST API")

;; Toggle between UIs
(defun my-toggle-usura-ui ()
  "Toggle between regular and org-mode UI."
  (interactive)
  (setq usura-use-org-mode (not usura-use-org-mode))
  (message "Usura %s UI enabled" 
           (if usura-use-org-mode "org-mode" "regular")))

;;; Usage in your config

;; Basic setup for init.el or config.el:
(use-package usura-mode
  :load-path "~/.config/doom/lisp/usura-mode"
  :commands (usura-claude-stream
             usura-claude-advice
             usura-claude-stream-org
             global-usura-mode)
  :init
  ;; Set up before loading
  (setq usura-verbose t
        usura-use-org-mode t)  ; Enable org-mode UI by default
  :config
  ;; After loading
  (global-usura-mode 1)
  ;; Customize UI
  (setq usura-chunk-size 8192))

;;; Advanced: Creating a custom Claude mode

(define-derived-mode my-claude-mode special-mode "My-Claude"
  "Custom mode for Claude interactions."
  (setq-local claude-stream--buffer "")
  (setq-local claude-stream--messages (make-hash-table :test 'equal))
  ;; Add custom key bindings
  (define-key my-claude-mode-map (kbd "g") #'usura-claude-stream)
  (define-key my-claude-mode-map (kbd "q") #'quit-window))

(provide 'usura-mode-example)
;;; usura-mode-example.el ends here
