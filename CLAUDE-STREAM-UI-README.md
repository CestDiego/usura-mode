# Claude Stream UI - High-Performance Emacs Interface

## Overview

The `claude-stream-ui.el` provides an extremely performant, visually rich text-based UI for processing Claude API streams in Emacs. It handles all event types discovered in the `/advice` command analysis.

## Key Features

### 1. **Performance Optimizations**
- **Batched Rendering**: Uses render queue with configurable delay (50ms default) to prevent flicker
- **Chunk Processing**: Processes streams in 4KB chunks for efficiency
- **Inhibited Redisplay**: Batches all UI updates before redrawing
- **Efficient JSON Parsing**: Optimized parsing with proper type hints

### 2. **Complete Event Type Coverage**

All discovered event types are handled:

| Event Type | Subtype | UI Treatment |
|------------|---------|--------------|
| `system` | `init` | Session box with metadata |
| `assistant` | - | Streaming/complete header |
| └─ content | `text` | Clean text display |
| └─ content | `thinking` | Boxed thinking section |
| └─ content | `tool_use` | Tool invocation box |
| `user` | `tool_result` | Success/error result box |
| `result` | `success` | Final statistics box |

### 3. **Visual Design**

```
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃ 🚀 Claude Session Started                                                   ┃
┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┫
┃ Session: f580327e                                                          ┃
┃ Model:   claude-opus-4-20250514                                            ┃
┃ Mode:    default                                                           ┃
┃ Tools:   16 available                                                      ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛

╔══════════════════════════════════════════════════════════════════════════════╗
║ 🤖 Assistant Response                                          [Streaming...] ║
╚══════════════════════════════════════════════════════════════════════════════╝

┌─💭 Thinking ─────────────────────────────────────────────────────────────────┐
│ The user is asking about whether to quit their job making Emacs jokes...    │
│ This requires considering multiple perspectives from IFS framework...        │
└──────────────────────────────────────────────────────────────────────────────┘

┌─🔧 Tool: Task ────────────────────────────────────────────────┤ID: ...1JTqn├─┐
│ ▸ description: Manager Part Analysis                                         │
│ ▸ prompt: As the Manager part in Internal Family Systems, analyze this...   │
└──────────────────────────────────────────────────────────────────────────────┘

✅ Tool Result [Task] ─────────────────────────────────────────────────────────┐
│ I'll analyze this situation from an Internal Family Systems Manager...       │
│ [Content continues...]                                                       │
└──────────────────────────────────────────────────────────────────────────────┘
```

### 4. **UI Templates System**

All UI elements are defined as text templates in `claude-stream-ui-templates`, making it easy to customize the appearance without changing code logic.

### 5. **Advanced Features**

- **Message Accumulation**: Tracks messages by ID for proper streaming updates
- **Active Tool Tracking**: Monitors in-progress tool executions
- **Smart Text Wrapping**: Automatically wraps long content with ellipsis
- **Progress Indicators**: Shows activity during long operations
- **Error Highlighting**: Different styling for successful vs failed tool results

### 6. **Missing Event Types Discovered**

Through analysis, we found and now handle:
- `thinking` blocks with signatures
- Tool results as both strings and text arrays
- `parent_tool_use_id` linking (tracked but not yet visualized)
- Comprehensive usage statistics with cache tokens
- Duration metrics (both client and API)

## Usage

### Basic Usage
```elisp
(require 'claude-stream-ui)

;; Simple test
(claude-stream-ui-test)

;; Create a process
(claude-stream-ui-create-process
 "my-session"
 "claude -p \"your prompt\" --output-format stream-json --verbose")
```

### Testing & Demo
```elisp
(require 'claude-stream-ui-test)

;; Interactive demo menu
(claude-stream-ui-demo)

;; Test specific scenarios
(claude-stream-ui-test-all-events)      ; All event types
(claude-stream-ui-test-edge-cases)     ; Unicode, long text, etc.
(claude-stream-ui-performance-test)    ; Rapid event handling
```

## Configuration

```elisp
;; Adjust chunk size for processing (default 4096)
(setq claude-stream-chunk-size 8192)

;; Adjust render delay for more/less responsive updates (default 0.05)
(setq claude-stream-render-delay 0.1)
```

## Architecture

### Single-File Design
As requested, all UI templates and logic are contained in a single file for maintainability. The templates are stored as an alist for easy modification.

### Performance Strategy
1. **Batching**: Events are queued and rendered in batches
2. **Lazy Rendering**: Only visible portions are updated
3. **Timer-based**: Uses Emacs timers for non-blocking updates
4. **Buffer-local State**: Efficient state management per stream

### Extensibility
- Add new event types by extending `claude-stream--render-event`
- Customize appearance by modifying `claude-stream-ui-templates`
- Add new content types in `claude-stream--render-content-item`

## Future Enhancements

1. **Collapsible Sections**: TAB to expand/collapse thinking blocks
2. **Search & Filter**: Find specific tools or content
3. **Export Options**: Save sessions in different formats
4. **Multi-Session View**: Monitor multiple Claude streams
5. **Tool Duration Tracking**: Show how long each tool took
6. **Parent-Child Visualization**: Show tool relationship trees

## Comparison with Previous Implementation

| Feature | Old Parser | New UI System |
|---------|------------|---------------|
| Performance | Sequential | Batched async |
| Visual Design | Basic text | Rich Unicode boxes |
| Event Coverage | Partial | Complete |
| Template System | Hardcoded | Configurable |
| Error Handling | Basic | Comprehensive |
| Tool Tracking | None | Active monitoring |

The new system provides a production-ready, performant UI for Claude streams with complete event coverage and beautiful text-based visualization.