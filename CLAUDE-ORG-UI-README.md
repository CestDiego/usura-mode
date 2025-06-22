# Claude Org-Mode UI

The org-mode UI provides a hierarchical, structured view of Claude's responses using Emacs org-mode formatting.

## Features

- **Hierarchical Structure**: Uses org-mode headings to organize content
- **Syntax Highlighting**: Wraps text responses in `#+begin_src markdown` blocks
- **Tool Visualization**: Clear separation of tool calls and results
- **Metadata Properties**: Stores session and message IDs as org properties
- **Native Org Features**: Folding, export, agenda integration, etc.

## Usage

### Enable Org-Mode UI

```elisp
;; Enable org-mode UI globally
(setq usura-use-org-mode t)

;; Or use the dedicated command (will prompt for input)
M-x usura-claude-stream-org
;; You'll see: "Claude prompt: " 
;; Enter your question and press RET
```

### How It Works

1. When you run any usura command, it prompts you for input
2. Your prompt is sent to the Claude CLI with streaming JSON output
3. The response is parsed and formatted in real-time into org-mode structure
4. The buffer updates live as Claude generates the response

### Key Commands

All commands interactively prompt for input before calling Claude:

- `C-c u s` - Prompts for a query, then starts Claude session (uses org-mode if enabled)
- `C-c u o` - Prompts for a query, then starts Claude session with org-mode UI explicitly
- `C-c u a` - Analyzes current buffer content (prompts for additional context)
- `C-c u r` - Prompts for instructions about the selected region

### Toggle Between UIs

```elisp
(setq usura-use-org-mode nil)  ; Use regular UI
(setq usura-use-org-mode t)    ; Use org-mode UI
```

## Output Structure

The org-mode UI creates structured output:

```org
#+TITLE: Claude Session - test
#+DATE: [2024-01-20 Sat 14:30]

* Claude Session :session:
:PROPERTIES:
:SESSION_ID: abc12345
:MODEL: claude-3-sonnet
:MODE: permissive
:TOOLS_COUNT: 5
:END:

** Assistant Response :streaming:
:PROPERTIES:
:MESSAGE_ID: msg_123
:END:

#+begin_src markdown
Here is my response with **markdown** formatting...
#+end_src

*** Thinking :thinking:
#+begin_quote
Internal reasoning process...
#+end_quote

*** Tool: file_read :tool:
:PROPERTIES:
:TOOL_ID: tool_456
:END:

Parameters:
- ~path~: /home/user/file.txt
- ~encoding~: utf-8

**** Result: file_read :success:result:
#+begin_example
File contents here...
#+end_example

** Session Summary :summary:
| Metric | Value |
|--------+-------|
| Duration | 1234 ms (API: 1000 ms) |
| Turns | 1 |
| Cost | $0.001234 |

*** Final Output :output:
#+begin_src markdown
Final formatted response...
#+end_src
```

## Benefits

1. **Export**: Use org-export to convert to HTML, PDF, etc.
2. **Navigation**: Use org-mode navigation commands
3. **Folding**: Collapse/expand sections with TAB
4. **Search**: Use org-mode search features
5. **Linking**: Create links between Claude sessions
6. **Tags**: Use org tags for categorization

## Configuration

```elisp
;; In your init.el or config.el
(use-package usura-mode
  :config
  (setq usura-use-org-mode t)        ; Enable org-mode UI
  (setq usura-verbose t)             ; Verbose output
  (global-usura-mode 1))
```

## Customization

The org-mode UI respects standard org-mode settings:

```elisp
;; Customize org appearance
(setq org-hide-leading-stars t)
(setq org-startup-indented t)
(setq org-src-fontify-natively t)
```

## Examples

### Basic Query
```elisp
;; This will prompt: "Claude prompt: "
(usura-claude-stream "Write a haiku about Emacs")

;; Interactive usage (M-x usura-claude-stream)
;; Will prompt you to enter your question
```

### Code Generation
```elisp
(usura-claude-stream "Create a Python function to sort a list")
```

### With Tools
```elisp
(usura-claude-stream "/file read ~/.emacs.d/init.el and explain the configuration")
```

## Tips

- Use `C-c C-e` to export the session to various formats
- Tag important responses for later reference
- Create custom org-capture templates for Claude sessions
- Use org-babel to execute code blocks from responses