;;; usura-claude-commands.el --- Dynamic command generation from ~/.claude/commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Automatically generates Emacs commands from Claude command files
;; Each .md file in ~/.claude/commands becomes a usura-claude-* command

;;; Code:

(require 'cl-lib)
(require 'usura-mode)

(defcustom usura-claude-commands-dir (expand-file-name "~/.claude/commands/")
  "Directory containing Claude command files."
  :type 'directory
  :group 'usura)

(defvar usura-claude-commands-alist nil
  "Alist of (command-name . command-file) pairs.")

(defvar usura-claude-commands-keymap (make-sparse-keymap)
  "Keymap for Claude commands.")

(defun usura-claude-commands--normalize-name (filename)
  "Convert FILENAME to a command name.
For example, 'advice.md' becomes 'advice'."
  (let ((name (file-name-sans-extension filename)))
    ;; Convert underscores/hyphens to be consistent
    (replace-regexp-in-string "[_-]" "-" name)))

(defun usura-claude-commands--make-function-name (command-name)
  "Create function name from COMMAND-NAME."
  (intern (format "usura-claude-%s" command-name)))

(defun usura-claude-commands--make-keybinding (command-name index)
  "Generate a keybinding for COMMAND-NAME using INDEX.
Uses C-c u c PREFIX where PREFIX is a letter or number."
  (let ((key (if (< index 26)
                 ;; Use letters a-z
                 (char-to-string (+ ?a index))
               ;; Then use numbers
               (format "%d" (- index 26)))))
    (kbd (format "C-c u c %s" key))))

(defmacro usura-define-claude-command (command-name command-file)
  "Define a command for COMMAND-NAME using COMMAND-FILE."
  (let* ((func-name (usura-claude-commands--make-function-name command-name))
         (doc-string (format "Execute Claude /%s command.
Automatically generated from %s" command-name command-file)))
    `(defun ,func-name (&optional arg)
       ,doc-string
       (interactive "P")
       (usura-claude-execute-command ,command-name ,command-file arg))))

(defun usura-claude-execute-command (command-name command-file &optional arg)
  "Execute Claude command COMMAND-NAME from COMMAND-FILE.
With prefix ARG, modify behavior (e.g., include region)."
  (let* ((content (cond
                   ;; If region is active, use it
                   ((use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end)))
                   ;; If ARG is given, use whole buffer
                   (arg
                    (buffer-substring-no-properties (point-min) (point-max)))
                   ;; Otherwise, prompt for content
                   (t
                    (read-string (format "Content for /%s command (or RET for current buffer): " 
                                       command-name)))))
         ;; If user hit RET without typing, use current buffer
         (content (if (string-empty-p content)
                      (buffer-substring-no-properties (point-min) (point-max))
                    content))
         ;; Truncate if too long
         (truncated (if (> (length content) 4000)
                        (concat (substring content 0 4000) "\n[... truncated ...]")
                      content))
         ;; Build the prompt with the command
         (prompt (format "/%s %s" command-name truncated))
         (command (usura-build-claude-command prompt)))
    
    ;; Use org-mode or regular UI based on preference
    (if (and usura-use-org-mode (featurep 'claude-stream-org-ui))
        (claude-stream-org-ui-create-process 
         (format "claude-%s" command-name) command)
      (claude-stream-ui-create-process 
       (format "claude-%s" command-name) command))))

(defun usura-claude-commands-scan ()
  "Scan ~/.claude/commands and create commands."
  (interactive)
  (setq usura-claude-commands-alist nil)
  
  ;; Clear existing bindings
  (setcdr usura-claude-commands-keymap nil)
  
  (when (file-directory-p usura-claude-commands-dir)
    (let ((files (directory-files usura-claude-commands-dir t "\\.md$"))
          (index 0))
      (dolist (file files)
        (let* ((basename (file-name-nondirectory file))
               (command-name (usura-claude-commands--normalize-name basename))
               (func-name (usura-claude-commands--make-function-name command-name))
               (keybinding (usura-claude-commands--make-keybinding command-name index)))
          
          ;; Store in alist
          (push (cons command-name file) usura-claude-commands-alist)
          
          ;; Create the function
          (eval `(usura-define-claude-command ,command-name ,file))
          
          ;; Add to keymap
          (define-key usura-claude-commands-keymap keybinding func-name)
          
          ;; Make it autoloadable
          (autoload func-name "usura-claude-commands" nil t)
          
          (cl-incf index))))
    
    (message "Loaded %d Claude commands" (length usura-claude-commands-alist))))

(defun usura-claude-commands-list ()
  "List all available Claude commands."
  (interactive)
  (if (null usura-claude-commands-alist)
      (message "No Claude commands found. Run `usura-claude-commands-scan' first.")
    (let ((buffer (get-buffer-create "*Claude Commands*")))
      (with-current-buffer buffer
        (erase-buffer)
        (insert "Claude Commands\n")
        (insert "==============\n\n")
        (insert "Key         Command              File\n")
        (insert "---         -------              ----\n")
        
        (let ((index 0))
          (dolist (entry (reverse usura-claude-commands-alist))
            (let* ((command-name (car entry))
                   (file (cdr entry))
                   (func-name (usura-claude-commands--make-function-name command-name))
                   (key (usura-claude-commands--make-keybinding command-name index)))
              (insert (format "%-11s %-20s %s\n" 
                             (key-description key)
                             (format "/%s" command-name)
                             (file-name-nondirectory file)))
              (cl-incf index))))
        
        (insert "\nUsage:\n")
        (insert "- Without region: prompts for content or uses current buffer\n")
        (insert "- With region: uses selected text\n")
        (insert "- With prefix arg (C-u): uses entire buffer\n")
        
        (special-mode)
        (goto-char (point-min)))
      (display-buffer buffer))))

;; Special handling for the existing advice command
(defun usura-claude-advice ()
  "Get Claude's advice about the current buffer.
This is a compatibility wrapper for the dynamically generated version."
  (interactive)
  ;; Check if the dynamic version exists
  (if (fboundp 'usura-claude-advice-dynamic)
      (call-interactively 'usura-claude-advice-dynamic)
    ;; Fallback to original implementation
    (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
           (truncated (if (> (length content) 4000)
                          (concat (substring content 0 4000) "\n[... truncated ...]")
                        content))
           (prompt (format "/advice about this content:\n\n%s" truncated))
           (command (usura-build-claude-command prompt)))
      (if (and usura-use-org-mode (featurep 'claude-stream-org-ui))
          (claude-stream-org-ui-create-process "claude-advice" command)
        (claude-stream-ui-create-process "claude-advice" command)))))

;; Interactive command chooser
(defun usura-claude-command ()
  "Choose and execute a Claude command interactively."
  (interactive)
  (if (null usura-claude-commands-alist)
      (progn
        (usura-claude-commands-scan)
        (if (null usura-claude-commands-alist)
            (message "No Claude commands found in %s" usura-claude-commands-dir)
          (usura-claude-command)))
    (let* ((choices (mapcar (lambda (entry)
                             (format "/%s" (car entry)))
                           usura-claude-commands-alist))
           (choice (completing-read "Claude command: " choices nil t))
           (command-name (substring choice 1))
           (func-name (usura-claude-commands--make-function-name command-name)))
      (if (fboundp func-name)
          (call-interactively func-name)
        (error "Command function %s not found" func-name)))))

;; Initialize on load
(defun usura-claude-commands-initialize ()
  "Initialize Claude commands system."
  (usura-claude-commands-scan)
  ;; Add the commands keymap to the main usura keymap
  (when (boundp 'usura-mode-map)
    (define-key usura-mode-map (kbd "C-c u c") usura-claude-commands-keymap)
    (define-key usura-mode-map (kbd "C-c u C") #'usura-claude-command)
    (define-key usura-mode-map (kbd "C-c u L") #'usura-claude-commands-list)))

;; Auto-initialize when loaded
(with-eval-after-load 'usura-mode
  (usura-claude-commands-initialize))

(provide 'usura-claude-commands)
;;; usura-claude-commands.el ends here