# Claude Dynamic Commands

Automatically generate Emacs commands from Claude command files in `~/.claude/commands/`.

## How It Works

Each `.md` file in `~/.claude/commands/` becomes an Emacs command:
- `advice.md` → `usura-claude-advice` → `/advice` command
- `explain.md` → `usura-claude-explain` → `/explain` command
- `my-custom.md` → `usura-claude-my-custom` → `/my-custom` command

## Usage

### Automatic Keybindings

Commands are automatically assigned keybindings:
- `C-c u c a` - First command (alphabetically)
- `C-c u c b` - Second command
- `C-c u c c` - Third command
- ... up to `C-c u c z`
- `C-c u c 0` - 27th command
- `C-c u c 1` - 28th command, etc.

### Interactive Commands

```elisp
;; Choose command from a list
M-x usura-claude-command RET
;; Or use: C-c u C

;; List all available commands
M-x usura-claude-commands-list RET
;; Or use: C-c u L

;; Use specific command (e.g., advice)
M-x usura-claude-advice RET
;; Or use: C-c u c a (if it's the first command)
```

### Command Behavior

Each command works with different contexts:

1. **No selection**: Prompts for content or uses current buffer
   ```
   M-x usura-claude-advice RET
   Content for /advice command (or RET for current buffer): [type or press RET]
   ```

2. **With region**: Uses selected text
   ```
   [Select some text]
   M-x usura-claude-advice RET
   ```

3. **With prefix arg** (`C-u`): Uses entire buffer without prompting
   ```
   C-u M-x usura-claude-advice RET
   ```

## Configuration

```elisp
;; Change commands directory (default: ~/.claude/commands/)
(setq usura-claude-commands-dir (expand-file-name "~/my-claude-commands/"))

;; Rescan commands directory
M-x usura-claude-commands-scan RET
```

## Creating New Commands

1. Create a new `.md` file in `~/.claude/commands/`:
   ```bash
   echo "# Explain Command" > ~/.claude/commands/explain.md
   ```

2. Rescan to load the new command:
   ```elisp
   M-x usura-claude-commands-scan RET
   ```

3. Use the new command:
   ```elisp
   M-x usura-claude-explain RET
   ```

## Examples

### Example 1: Code Review
```bash
# Create review command
echo "# Code Review" > ~/.claude/commands/review.md
```

In Emacs:
```elisp
;; Select code to review
M-x usura-claude-review RET
```

### Example 2: Documentation
```bash
# Create docs command
echo "# Generate Documentation" > ~/.claude/commands/docs.md
```

In Emacs:
```elisp
;; With cursor in a function
C-u M-x usura-claude-docs RET
```

## Features

- **Dynamic Loading**: Commands are generated at runtime
- **Automatic Keybindings**: No manual configuration needed
- **Context Aware**: Works with regions, buffers, or prompts
- **Org-Mode Support**: Respects `usura-use-org-mode` setting
- **Backward Compatible**: Existing `usura-claude-advice` still works

## Troubleshooting

### Commands Not Found
```elisp
;; Check if commands were loaded
M-x usura-claude-commands-list RET

;; Manually scan
M-x usura-claude-commands-scan RET
```

### Keybinding Conflicts
The system uses `C-c u c [letter/number]` pattern. To see all bindings:
```elisp
M-x usura-claude-commands-list RET
```

## Integration

The dynamic commands integrate with:
- Org-mode UI (when enabled)
- Project explorer
- All existing usura-mode features

## Tips

1. **Naming**: Use descriptive names for command files
2. **Organization**: Group related commands with prefixes
3. **Documentation**: Add descriptions in the .md files
4. **Shortcuts**: Most used commands get early letters (a, b, c...)