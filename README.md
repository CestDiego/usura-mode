# Usura Mode - Claude AI Integration for Emacs

Beautiful, high-performance Claude AI integration for Emacs with streaming support and rich text-based UI.

## Features

- 🚀 **High-Performance Streaming**: Async JSON stream processing that doesn't block Emacs
- 🎨 **Beautiful Text UI**: Unicode box-drawing with visual indicators for different content types
- 📝 **Org-Mode UI**: Alternative hierarchical view using org-mode structure
- 🔧 **Full Tool Support**: Visualizes all Claude tool uses and results
- 💭 **Thinking Blocks**: Shows Claude's reasoning process
- 📊 **Complete Event Coverage**: Handles all Claude stream event types
- ⚡ **Optimized Rendering**: Batched updates with configurable delays
- 💬 **Interactive Prompting**: All commands prompt for your input

## Installation

### Doom Emacs

Add to your `packages.el`:
```elisp
(package! usura-mode
  :recipe (:local-repo "~/.config/doom/lisp/usura-mode"))
```

Then in `config.el`:
```elisp
(use-package! usura-mode
  :commands (usura-claude-stream
             usura-claude-advice
             usura-claude-region
             global-usura-mode)
  :init
  (global-usura-mode 1))
```

### Manual Installation

```elisp
(add-to-list 'load-path "~/.config/doom/lisp/usura-mode")
(require 'usura-mode)
(global-usura-mode 1)
```

## Usage

### Interactive Commands

All commands prompt you for input before sending to Claude:

| Command | Key Binding | Description |
|---------|-------------|-------------|
| `usura-claude-stream` | `C-c u s` | Prompts for a query, then starts a Claude conversation |
| `usura-claude-advice` | `C-c u a` | Analyzes current buffer (asks for specific advice) |
| `usura-claude-region` | `C-c u r` | Prompts for instructions about selected region |
| `usura-claude-insert-response` | `C-c u i` | Prompts for query, inserts response at point |
| `usura-claude-test-ui` | `C-c u t` | Test UI with demo data |
| `usura-claude-stream-org` | `C-c u o` | Prompts for query, uses org-mode UI |
| `claude-project-explorer` | `C-c u p` | Browse Claude project files |
| `claude-project-open-jsonl-file` | `C-c u f` | Open specific JSONL file |
| `usura-claude-command` | `C-c u C` | Choose from available Claude commands |
| `usura-claude-commands-list` | `C-c u L` | List all Claude commands |
| Dynamic commands | `C-c u c a-z,0-9` | Commands from ~/.claude/commands/ |

### Examples

```elisp
;; Start a conversation
M-x usura-claude-stream RET
Claude prompt: Explain quantum computing RET
;; Claude's response appears in a new buffer with live streaming

;; Get advice about your code
M-x usura-claude-advice RET  
;; Automatically includes current buffer content
;; You can add specific questions in the prompt

;; Analyze selected region
;; 1. Select some code
;; 2. M-x usura-claude-region RET
;; 3. Claude prompt for region: What does this function do? RET
```

## UI Overview

### Session Box
```
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ 🚀 Claude Session Started                                                   ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ Session: 8113cd23                                                          ┃
┃ Model:   claude-opus-4-20250514                                            ┃
┃ Mode:    default                                                           ┃
┃ Tools:   16 available                                                      ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
```

### Content Types

- **Text**: Clean display of Claude's responses
- **Thinking** 💭: Shows Claude's reasoning in a dedicated box
- **Tool Use** 🔧: Visualizes tool invocations with parameters
- **Tool Results** ✅/❌: Success or error results from tools
- **Statistics** 📊: Final usage metrics and costs

## Org-Mode UI

Usura-mode includes an alternative UI that uses org-mode formatting:

### Features
- Hierarchical structure with org headings
- Text responses wrapped in `#+begin_src markdown` blocks for syntax highlighting
- Tool calls and results in org-mode blocks
- Metadata stored as org properties
- Full org-mode functionality (folding, export, links, etc.)

### Usage
```elisp
;; Enable globally
(setq usura-use-org-mode t)

;; Or use dedicated command
M-x usura-claude-stream-org RET
```

See [CLAUDE-ORG-UI-README.md](CLAUDE-ORG-UI-README.md) for detailed documentation.

## Project File Explorer

Browse and view Claude project files from `~/.claude/projects/`:

```elisp
;; Open project explorer
M-x claude-project-explorer RET

;; Open specific JSONL file
M-x claude-project-open-jsonl-file RET
```

Features:
- Browse all Claude projects in an interactive buffer
- Parse JSONL files and display in org-mode format
- Smart caching for fast repeated access
- Automatic cache invalidation when files change

See [CLAUDE-PROJECT-EXPLORER-README.md](CLAUDE-PROJECT-EXPLORER-README.md) for details.

## Dynamic Claude Commands

Automatically generate commands from `~/.claude/commands/` directory:

```elisp
;; Each .md file becomes a command:
;; advice.md → M-x usura-claude-advice (C-c u c a)
;; explain.md → M-x usura-claude-explain (C-c u c b)

;; Choose from available commands
M-x usura-claude-command RET

;; List all commands with keybindings
M-x usura-claude-commands-list RET
```

Features:
- Automatic function generation from command files
- Smart keybinding assignment (C-c u c a-z, then 0-9)
- Context-aware (region, buffer, or prompt)
- Integrates with org-mode UI

See [CLAUDE-COMMANDS-README.md](CLAUDE-COMMANDS-README.md) for details.

## Configuration

```elisp
;; Customize Claude executable path
(setq usura-claude-executable "claude")

;; Output format (stream-json, json, or text)
(setq usura-default-output-format "stream-json")

;; Verbose output
(setq usura-verbose t)

;; Enable org-mode UI (default: t)
(setq usura-use-org-mode t)

;; Performance tuning
(setq claude-stream-chunk-size 8192)      ; Larger buffer chunks
(setq claude-stream-render-delay 0.1)    ; Slower updates for less flicker
```

## Architecture

```
usura-mode/
├── usura-mode.el              # Main entry point and commands
├── claude-stream-ui.el        # High-performance UI system
├── claude-stream-org-ui.el    # Org-mode based UI system
├── claude-project-explorer.el # Browse and view project files
├── usura-claude-commands.el   # Dynamic command generation
├── claude-stream-parser.el    # Legacy parser (compatibility)
├── json-stream-parser.el      # General JSON streaming
└── claude-stream-test.el      # Testing utilities
```

## Package Structure & Imports

### Loading Components

```elisp
;; Method 1: Load everything (recommended)
(require 'usura-mode)

;; Method 2: Load specific components
(require 'usura-mode)
(require 'claude-stream-ui)      ; Just the UI system
(require 'json-stream-parser)    ; Just JSON parsing
(require 'claude-stream-parser)  ; Legacy parser

;; Method 3: Load all components at once
(usura-load-all)

;; Method 4: Load test modules
(usura-load-tests)
```

### Available Modules

| Module | Purpose | Main Functions |
|--------|---------|----------------|
| `usura-mode` | Main entry point | All user commands |
| `claude-stream-ui` | UI rendering system | `claude-stream-ui-create-process` |
| `claude-stream-org-ui` | Org-mode UI system | `claude-stream-org-ui-create-process` |
| `json-stream-parser` | Generic JSON streaming | `json-stream-process-filter` |
| `claude-stream-parser` | Legacy parser | `claude-stream-create-process` |
| `claude-stream-ui-test` | Testing utilities | `claude-stream-ui-demo` |
| `claude-stream-visual-test` | Visual testing | `claude-stream-visual-test` |

## Advanced Usage

### Programmatic Access

```elisp
;; Access UI templates for customization
(setq my-template (alist-get 'session-init usura-ui-templates))

;; Configure performance
(setq usura-chunk-size 8192)      ; or claude-stream-chunk-size
(setq usura-render-delay 0.1)     ; or claude-stream-render-delay

;; Use the API directly
(let ((command (usura-build-claude-command "Hello")))
  (usura-create-stream-process "my-session" command))

;; Parse JSON lines
(usura-parse-json-line "{\"type\":\"assistant\"}")

;; Custom rendering
(usura-render-event '((type . "system") (subtype . "init")))

;; List active sessions
(usura-list-sessions)

;; Kill all sessions
(usura-kill-all-sessions)
```

### Org Mode Integration

```elisp
;; Create a Claude source block
M-x usura-org-claude-block RET
Claude prompt: Explain this concept
```

## Testing

```elisp
;; Test with sample data
M-x usura-claude-test-ui

;; Test with the advice file
M-x usura-claude-test-advice-file

;; Visual comparison test
(require 'claude-stream-visual-test)
(claude-stream-visual-test)
```

## Performance

- **Async Processing**: Never blocks Emacs UI
- **Batched Rendering**: Updates every 50ms by default
- **Efficient Parsing**: Optimized JSON handling
- **Chunk Processing**: 4KB chunks for smooth streaming

## Event Types Supported

| Event | Description | UI Treatment |
|-------|-------------|--------------|
| `system.init` | Session start | Session info box |
| `assistant.text` | Text response | Clean text display |
| `assistant.thinking` | Reasoning | Thinking box with 💭 |
| `assistant.tool_use` | Tool invocation | Tool box with 🔧 |
| `user.tool_result` | Tool output | Result box ✅/❌ |
| `result` | Final stats | Statistics box 📊 |

## Troubleshooting

### No output appearing
- Check that `claude` is in your PATH
- Verify with: `M-x shell RET claude --version RET`

### Garbled characters
- Ensure your Emacs supports Unicode
- Try: `(set-language-environment "UTF-8")`

### Performance issues
- Increase `claude-stream-render-delay` to 0.2
- Reduce `claude-stream-chunk-size` to 2048

## Contributing

1. Test new event types with `claude-stream-visual-test`
2. Add templates to `claude-stream-ui-templates`
3. Update rendering in `claude-stream--render-event`

## License

GPL-3.0 or later

## Acknowledgments

- Built for the Claude SDK
- Inspired by gptel, org-ai, and other Emacs AI packages
- Unicode box drawing from various TUI best practices