# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is an Emacs configuration directory. Emacs configuration is typically written in Emacs Lisp (.el files) and organized in various ways depending on the user's preferences.

## Common File Structure

Emacs configurations typically follow one of these patterns:

1. **Single init.el**: All configuration in `init.el` or `~/.emacs`
2. **Modular approach**: `init.el` loads modules from subdirectories (e.g., `lisp/`, `modules/`, `config/`)
3. **early-init.el**: Emacs 27+ supports `early-init.el` for early initialization (before package system and GUI)
4. **org-mode literate config**: Configuration in `config.org` or `README.org` with code blocks tangled to .el files

## Common Commands

### Byte-compile Emacs Lisp files
```bash
emacs --batch -f batch-byte-compile file.el
```

### Byte-compile entire directory
```bash
emacs --batch --eval "(byte-recompile-directory \"~/.config/emacs\" 0)"
```

### Test configuration without affecting running instance
```bash
emacs -Q --load init.el
```

### Start Emacs with specific configuration directory
```bash
emacs --init-directory ~/.config/emacs
```

## Package Management

Emacs has several package managers. Check `init.el` or `early-init.el` to determine which is in use:

- **package.el**: Built-in package manager (ELPA, MELPA)
- **straight.el**: Declarative package management with git repos
- **use-package**: Macro for configuring and lazy-loading packages (works with both)
- **elpaca**: Modern asynchronous package manager

## Development Guidelines

### File Organization
- Keep `init.el` or `early-init.el` as the entry point
- Group related configurations together (e.g., all programming modes, all UI settings)
- Use `require` or `load` for custom modules in subdirectories

### Performance Considerations
- Use `use-package` with `:defer` for lazy loading
- Byte-compile files for faster loading
- Profile startup time with `esup` package or built-in profiler

### Elisp Conventions
- Use `defcustom` for user-configurable variables
- Use `defvar` for internal variables
- Prefix custom functions/variables with namespace (e.g., `my/function-name`)
- Add docstrings to functions and variables
