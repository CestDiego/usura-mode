;;; claude-project-explorer.el --- Explore Claude project JSONL files -*- lexical-binding: t; -*-

;;; Commentary:
;; Browse and display Claude project files from ~/.claude/projects/
;; Parses JSONL files and displays them in org-mode format with caching

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'org)
(require 'claude-stream-org-ui)

(defcustom claude-projects-dir (expand-file-name "~/.claude/projects/")
  "Directory containing Claude project files."
  :type 'directory
  :group 'claude)

(defcustom claude-project-cache-dir (expand-file-name "~/.emacs.d/claude-cache/")
  "Directory for cached org-mode representations of Claude projects."
  :type 'directory
  :group 'claude)

;; Ensure cache directory exists
(unless (file-exists-p claude-project-cache-dir)
  (make-directory claude-project-cache-dir t))

(defvar claude-project-explorer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'claude-project-explorer-open)
    (define-key map (kbd "r") #'claude-project-explorer-refresh)
    (define-key map (kbd "d") #'claude-project-explorer-delete-cache)
    (define-key map (kbd "g") #'claude-project-explorer-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for Claude project explorer.")

(define-derived-mode claude-project-explorer-mode special-mode "Claude-Projects"
  "Major mode for browsing Claude project files."
  (setq truncate-lines t)
  (setq buffer-read-only t))

;;;###autoload
(defun claude-project-explorer ()
  "Open Claude project explorer."
  (interactive)
  (let ((buffer (get-buffer-create "*Claude Projects*")))
    (with-current-buffer buffer
      (claude-project-explorer-mode)
      (claude-project-explorer-refresh))
    (switch-to-buffer buffer)))

(defun claude-project-explorer-refresh ()
  "Refresh the project list."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert "Claude Projects\n")
    (insert "==============\n\n")
    (insert "Press RET to open, 'r' to refresh, 'd' to delete cache, 'q' to quit\n\n")
    
    (if (file-exists-p claude-projects-dir)
        (let ((projects (claude-project-explorer--find-projects)))
          (if projects
              (dolist (project projects)
                (claude-project-explorer--insert-project-entry project))
            (insert "No projects found.\n")))
      (insert (format "Projects directory not found: %s\n" claude-projects-dir)))))

(defun claude-project-explorer--find-projects ()
  "Find all JSONL files in the projects directory."
  (let ((files '()))
    (dolist (dir (directory-files claude-projects-dir t "^[^.]"))
      (when (file-directory-p dir)
        (dolist (file (directory-files dir t "\\.jsonl$"))
          (push (list :file file
                      :project (file-name-nondirectory dir)
                      :name (file-name-sans-extension (file-name-nondirectory file))
                      :mtime (nth 5 (file-attributes file)))
                files))))
    (sort files (lambda (a b) 
                  (time-less-p (plist-get b :mtime) 
                              (plist-get a :mtime))))))

(defun claude-project-explorer--insert-project-entry (project)
  "Insert a project entry for PROJECT."
  (let* ((file (plist-get project :file))
         (project-name (plist-get project :project))
         (name (plist-get project :name))
         (mtime (plist-get project :mtime))
         (cached-p (claude-project-explorer--cache-exists-p file)))
    (insert (propertize (format "%-20s %-30s %s %s\n"
                                project-name
                                name
                                (format-time-string "%Y-%m-%d %H:%M" mtime)
                                (if cached-p "[cached]" ""))
                        'claude-project-file file
                        'face 'link
                        'mouse-face 'highlight))))

(defun claude-project-explorer-open ()
  "Open the project at point."
  (interactive)
  (let ((file (get-text-property (point) 'claude-project-file)))
    (when file
      (claude-project-explorer-open-file file))))

(defun claude-project-explorer-open-file (jsonl-file)
  "Open JSONL-FILE and display it in org-mode format."
  (let* ((cache-file (claude-project-explorer--cache-filename jsonl-file))
         (jsonl-mtime (nth 5 (file-attributes jsonl-file)))
         (cache-mtime (and (file-exists-p cache-file)
                          (nth 5 (file-attributes cache-file)))))
    
    ;; Use cache if it exists and is newer than the source
    (if (and cache-mtime (time-less-p jsonl-mtime cache-mtime))
        (find-file cache-file)
      ;; Otherwise parse and create new org buffer
      (claude-project-explorer--parse-and-display jsonl-file))))

(defun claude-project-explorer--cache-filename (jsonl-file)
  "Get cache filename for JSONL-FILE."
  (let* ((relative (file-relative-name jsonl-file claude-projects-dir))
         (cache-name (replace-regexp-in-string "/" "_" relative)))
    (expand-file-name (concat cache-name ".org") claude-project-cache-dir)))

(defun claude-project-explorer--cache-exists-p (jsonl-file)
  "Check if cache exists for JSONL-FILE."
  (file-exists-p (claude-project-explorer--cache-filename jsonl-file)))

(defun claude-project-explorer--parse-and-display (jsonl-file)
  "Parse JSONL-FILE and display in org-mode."
  (let* ((buffer-name (format "*Claude: %s*" 
                             (file-name-sans-extension 
                              (file-name-nondirectory jsonl-file))))
         (buffer (get-buffer-create buffer-name))
         (cache-file (claude-project-explorer--cache-filename jsonl-file)))
    
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      
      ;; Set up org-mode preferences
      (setq-local org-hide-leading-stars t)
      (setq-local org-startup-indented t)
      (setq-local org-src-fontify-natively t)
      (setq-local org-fontify-quote-and-verse-blocks t)
      (setq-local org-startup-folded nil)
      
      ;; Initialize state variables
      (setq-local claude-stream-org--messages (make-hash-table :test 'equal))
      (setq-local claude-stream-org--active-tools (make-hash-table :test 'equal))
      (setq-local claude-stream-org--in-src-block nil)
      
      ;; Insert header
      (insert "#+TITLE: Claude Session - " (file-name-nondirectory jsonl-file) "\n")
      (insert "#+DATE: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n")
      (insert "#+OPTIONS: toc:nil\n")
      (insert "#+PROPERTY: JSONL_FILE " jsonl-file "\n\n")
      
      ;; Parse and render JSONL content
      (message "Parsing %s..." jsonl-file)
      (claude-project-explorer--parse-jsonl-file jsonl-file buffer)
      
      ;; Save to cache
      (write-region (point-min) (point-max) cache-file)
      (message "Saved cache to %s" cache-file))
    
    (switch-to-buffer buffer)
    (goto-char (point-min))))

(defun claude-project-explorer--parse-jsonl-file (file buffer)
  "Parse JSONL FILE and render to BUFFER."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((line-count 0))
      (while (not (eobp))
        (let ((line-start (point)))
          (forward-line 1)
          (let ((line (buffer-substring-no-properties line-start (1- (point)))))
            (when (> (length line) 0)
              (cl-incf line-count)
              (condition-case err
                  (let* ((json-object-type 'alist)
                         (json-array-type 'list)
                         (json-key-type 'string)
                         (data (json-read-from-string line)))
                    (with-current-buffer buffer
                      (save-excursion
                        (goto-char (point-max))
                        (claude-stream-org--render-event data))))
                (error
                 (message "Error parsing line %d: %s" line-count err))))))))
    
    ;; Ensure any open src blocks are closed
    (with-current-buffer buffer
      (when (bound-and-true-p claude-stream-org--in-src-block)
        (goto-char (point-max))
        (insert "#+end_src\n")
        (setq claude-stream-org--in-src-block nil)))))

(defun claude-project-explorer-delete-cache ()
  "Delete cache for the project at point."
  (interactive)
  (let ((file (get-text-property (point) 'claude-project-file)))
    (when file
      (let ((cache-file (claude-project-explorer--cache-filename file)))
        (when (file-exists-p cache-file)
          (delete-file cache-file)
          (message "Deleted cache for %s" (file-name-nondirectory file))
          (claude-project-explorer-refresh))))))

;; Interactive command to open a specific JSONL file
;;;###autoload
(defun claude-project-open-jsonl-file (file)
  "Open a Claude JSONL FILE directly."
  (interactive "fOpen Claude JSONL file: ")
  (if (string-match "\\.jsonl$" file)
      (claude-project-explorer-open-file file)
    (error "Not a JSONL file: %s" file)))

;; Command to clear all caches
;;;###autoload
(defun claude-project-clear-all-caches ()
  "Clear all cached org files."
  (interactive)
  (when (yes-or-no-p "Clear all Claude project caches? ")
    (dolist (file (directory-files claude-project-cache-dir t "\\.org$"))
      (delete-file file))
    (message "Cleared all caches")))

(provide 'claude-project-explorer)
;;; claude-project-explorer.el ends here