# Usura-Mode Technical Architecture
## A First-Principles Analysis of Stream Processing and UI Design in Emacs

### Table of Contents
1. [Introduction](#introduction)
2. [Core Philosophy](#core-philosophy)
3. [Stream Processing Architecture](#stream-processing-architecture)
4. [UI Rendering Systems](#ui-rendering-systems)
5. [Performance Engineering](#performance-engineering)
6. [Extensibility and Plugin Architecture](#extensibility-and-plugin-architecture)
7. [Implementation Details](#implementation-details)
8. [Design Patterns and Idioms](#design-patterns-and-idioms)
9. [Future Considerations](#future-considerations)

## Introduction

Usura-mode is an Emacs package that provides deep integration with Claude AI through a sophisticated stream processing pipeline and multiple UI paradigms. This document describes the technical architecture from first principles, explaining not just what we built, but why we built it this way.

## Core Philosophy

Our design philosophy centers on several key principles:

1. **Non-blocking Operation**: Emacs must remain responsive during high-throughput streaming
2. **Visual Clarity**: Information should be presented in a scannable, hierarchical format
3. **Extensibility**: Users should be able to extend the system without modifying core code
4. **Performance**: Handle megabytes of streaming JSON without degradation
5. **Choice**: Provide multiple UI paradigms for different workflows

## Stream Processing Architecture

### The Pipeline

```
┌─────────────┐    ┌──────────────┐    ┌─────────────┐    ┌──────────────┐
│   Claude    │───▶│   Process    │───▶│    JSON     │───▶│    Event     │
│   Binary    │    │   Filter     │    │   Parser    │    │    Queue     │
└─────────────┘    └──────────────┘    └─────────────┘    └──────────────┘
                           │                                        │
                           ▼                                        ▼
                   ┌──────────────┐                        ┌──────────────┐
                   │    Buffer    │                        │   Batch      │
                   │ Accumulator  │                        │  Renderer    │
                   └──────────────┘                        └──────────────┘
                                                                   │
                                                                   ▼
                                                           ┌──────────────┐
                                                           │   Display    │
                                                           │   Buffer     │
                                                           └──────────────┘
```

### Process Management

We use Emacs' built-in async process facilities:

```elisp
(start-process-shell-command name buffer command)
```

Key decisions:
- **Shell command execution**: Allows proper quoting and piping
- **Dedicated buffers**: Each session gets its own process buffer
- **Filter functions**: Custom handlers process output incrementally

### Stream Parsing

The JSON stream parser handles newline-delimited JSON (NDJSON):

```elisp
;; Accumulate partial data
(setq buffer (concat buffer new-data))

;; Extract complete lines
(while (string-match "\n" buffer)
  (let ((line (substring buffer 0 (match-beginning 0))))
    (process-json-line line))
  (setq buffer (substring buffer (match-end 0))))
```

This approach:
- Handles partial JSON objects across network packets
- Maintains minimal memory footprint
- Processes data as soon as complete lines are available

### Event Queue and Batching

To prevent UI flicker during rapid updates:

```elisp
(defvar-local render-queue nil)
(defvar-local render-timer nil)

(defun queue-render (data)
  (push data render-queue)
  (unless render-timer
    (setq render-timer
          (run-with-timer 0.05 nil #'flush-render-queue))))
```

Benefits:
- Groups multiple updates into single redraw
- Reduces CPU usage
- Maintains visual stability
- User-configurable delay

## UI Rendering Systems

### Text-Based UI

The primary UI uses Unicode box-drawing characters for structure:

```
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ 🚀 Claude Session Started                                                   ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ Session: 8113cd23                                                          ┃
┃ Model:   claude-opus-4                                                     ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

Template system allows customization without code changes:

```elisp
(defconst claude-stream-ui-templates
  '((session-init . "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ 🚀 Claude Session Started                                                   ┃
...")))
```

### Org-Mode UI

Alternative UI leveraging org-mode's features:

```org
* Claude Session :session:
:PROPERTIES:
:SESSION_ID: abc123
:MODEL: claude-3-sonnet
:END:

** Assistant Response :streaming:
#+begin_src markdown
Response content here...
#+end_src
```

Advantages:
- Native folding and navigation
- Export to multiple formats
- Syntax highlighting via org-babel
- Property drawers for metadata

### Renderer Architecture

Both UIs implement the same interface:

```elisp
(defun render-event (data)
  (let ((type (alist-get "type" data)))
    (cond
     ((string= type "system") (render-system data))
     ((string= type "assistant") (render-assistant data))
     ((string= type "user") (render-user data))
     ((string= type "result") (render-result data)))))
```

This allows:
- Easy addition of new renderers
- Consistent event handling
- Code reuse across UIs

## Performance Engineering

### Memory Management

1. **Buffer-local variables**: Isolate state per session
2. **Hash tables**: O(1) lookups for messages and tools
3. **Lazy evaluation**: Process only visible content
4. **Garbage collection**: Clear completed tool tracking

### Rendering Optimization

```elisp
(let ((inhibit-redisplay t))
  ;; Batch all rendering operations
  (save-excursion
    (dolist (data queue)
      (render-event data))))
(redisplay)
```

This pattern:
- Prevents intermediate redraws
- Batches all DOM-like operations
- Forces single repaint at end

### Chunk Size Tuning

```elisp
(defcustom claude-stream-chunk-size 4096
  "Buffer chunk size for stream processing."
  :type 'integer)
```

Larger chunks:
- Reduce system calls
- Improve throughput
- May increase latency

### Progress Indicators

For long operations:

```elisp
(defconst progress-chars "⣾⣽⣻⢿⡿⣟⣯⣷")
(defun show-progress ()
  (nth (% (cl-incf position) 8) progress-chars))
```

Non-blocking visual feedback without timers.

## Extensibility and Plugin Architecture

### Dynamic Command System

The command system demonstrates our extensibility approach:

```elisp
(defmacro usura-define-claude-command (command-name command-file)
  `(defun ,(intern (format "usura-claude-%s" command-name)) (&optional arg)
     ,(format "Execute Claude /%s command." command-name)
     (interactive "P")
     (usura-claude-execute-command ,command-name ,command-file arg)))
```

Key features:
1. **Filesystem-based discovery**: Commands are files, not code
2. **Automatic function generation**: No manual registration
3. **Consistent interface**: All commands work the same way
4. **Dynamic keybindings**: Auto-assigned based on discovery order

### Project Explorer

Demonstrates our approach to auxiliary features:

```elisp
;; Cache parsed files for performance
(defun cache-filename (jsonl-file)
  (expand-file-name 
    (concat (replace-regexp-in-string "/" "_" relative) ".org")
    cache-dir))

;; Check freshness before using cache
(if (time-less-p jsonl-mtime cache-mtime)
    (find-file cache-file)
  (parse-and-display jsonl-file))
```

Design principles:
- Cache expensive operations
- Validate cache freshness
- Provide manual refresh
- Transparent to user

## Implementation Details

### Error Handling

Defensive programming throughout:

```elisp
(condition-case err
    (json-read-from-string line)
  (error
   (message "[Stream] JSON parse error: %s" err)
   nil))
```

Never crash on bad input.

### Process Lifecycle

```elisp
(set-process-sentinel proc
  (lambda (p e)
    (with-current-buffer (process-buffer p)
      (goto-char (point-max))
      (insert "\n[Stream ended: " e "]\n"))))
```

Clean shutdown and status reporting.

### State Management

Buffer-local state for isolation:

```elisp
(defvar-local claude-stream--buffer "")
(defvar-local claude-stream--messages (make-hash-table :test 'equal))
(defvar-local claude-stream--active-tools (make-hash-table :test 'equal))
```

Each session maintains independent state.

## Design Patterns and Idioms

### Template Method Pattern

UI renderers follow template method:

```elisp
;; Abstract operation
(defun render-event (data) ...)

;; Concrete implementations
(defun text-ui-render-event (data) ...)
(defun org-ui-render-event (data) ...)
```

### Observer Pattern

Event streaming is essentially observer:

```elisp
;; Subject: JSON parser
;; Observers: UI renderers
(dolist (line lines)
  (when-let ((data (parse-json-line line)))
    (notify-observers data)))
```

### Factory Pattern

Dynamic command generation:

```elisp
;; Factory method
(defun create-command (name file)
  (eval `(defun ,(make-function-name name) ()
           ...)))
```

### Strategy Pattern

Pluggable UI systems:

```elisp
(if use-org-mode
    (org-ui-create-process ...)
  (text-ui-create-process ...))
```

## Future Considerations

### Potential Enhancements

1. **WebSocket Support**: Direct streaming without process overhead
2. **Incremental Parsing**: Stream JSON parsing for large objects
3. **Virtual Scrolling**: Handle multi-megabyte sessions
4. **Parallel Rendering**: Utilize Emacs 28+ threading
5. **LSP Integration**: Language server protocol for code analysis

### Scalability Paths

1. **Sharding**: Split large sessions across multiple buffers
2. **Compression**: Store historical data compressed
3. **Indexing**: Full-text search across sessions
4. **Clustering**: Distribute processing across Emacs instances

### API Stability

Current API surface:
- User commands: Stable
- Rendering functions: Internal, may change
- Event format: Follows Claude's format
- Extension points: Stable macros and hooks

## Conclusion

Usura-mode demonstrates that sophisticated stream processing and rich UIs are possible in Emacs without sacrificing performance or responsiveness. The architecture's emphasis on:

- Event-driven processing
- Batched rendering  
- Pluggable components
- Template-based customization
- Dynamic extensibility

Creates a system that is both powerful for users and maintainable for developers. The codebase serves as a reference implementation for building high-performance, streaming applications in Emacs Lisp.

The key insight is that by respecting Emacs' event loop, leveraging buffer-local state, and batching operations, we can handle real-time streams that would challenge even modern web applications, all within a text editor from 1976.

This architecture is not just about making Claude work in Emacs—it's a blueprint for integrating any streaming API into Emacs with excellent performance and user experience.