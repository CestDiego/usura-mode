# Claude Project Explorer

Browse and view Claude project files stored in `~/.claude/projects/` with org-mode formatting and caching.

## Features

- **Project Browser**: Interactive buffer to browse all Claude projects
- **JSONL Parsing**: Converts JSONL streams to org-mode format
- **Smart Caching**: Caches parsed org files for fast access
- **Live Updates**: Regenerates cache when source files change

## Usage

### Browse Projects

```elisp
M-x claude-project-explorer RET
```

This opens a buffer listing all projects. Key bindings:
- `RET` - Open project file
- `r` or `g` - Refresh list
- `d` - Delete cache for file at point
- `q` - Quit

### Open Specific File

```elisp
M-x claude-project-open-jsonl-file RET
/path/to/file.jsonl RET
```

Or use the key binding: `C-c u f`

### Clear All Caches

```elisp
M-x claude-project-clear-all-caches RET
```

## Configuration

```elisp
;; Change projects directory (default: ~/.claude/projects/)
(setq claude-projects-dir (expand-file-name "~/my-claude-projects/"))

;; Change cache directory (default: ~/.emacs.d/claude-cache/)
(setq claude-project-cache-dir (expand-file-name "~/.cache/claude/"))
```

## File Structure

Claude projects are organized as:
```
~/.claude/projects/
├── project-name-1/
│   ├── session1.jsonl
│   └── session2.jsonl
└── project-name-2/
    └── session1.jsonl
```

## Org-Mode Output

The explorer converts JSONL streams into structured org-mode:

```org
#+TITLE: Claude Session - session1.jsonl
#+DATE: [2024-01-20 Sat 15:30]
#+PROPERTY: JSONL_FILE /path/to/session1.jsonl

* Claude Session :session:
:PROPERTIES:
:SESSION_ID: abc123
:MODEL: claude-3-sonnet
:END:

** Assistant Response :complete:
#+begin_src markdown
Response content here...
#+end_src

*** Tool: file_read :tool:
:PROPERTIES:
:TOOL_ID: tool123
:END:

Parameters:
- ~path~: /home/user/file.txt
```

## Tips

1. **Performance**: Cached files load instantly
2. **Search**: Use org-mode search in parsed files
3. **Export**: Export sessions to HTML, PDF, etc.
4. **Archive**: Move old projects out of the main directory

## Integration with Usura Mode

The explorer is fully integrated:
- `C-c u p` - Open project explorer
- `C-c u f` - Open specific JSONL file
- Uses same org-mode rendering as live sessions

## Troubleshooting

### Empty Output
If you see only the header, check:
1. File exists and contains valid JSONL
2. Each line is valid JSON
3. Check `*Claude Debug*` buffer for errors

### Cache Issues
- Delete individual cache with `d` in explorer
- Clear all with `M-x claude-project-clear-all-caches`
- Caches auto-refresh when source is newer