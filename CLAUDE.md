# CLAUDE.md

This file provides comprehensive guidance to Claude Code when working with this Emacs configuration.

## Overview

This is a **modern, hand-rolled Emacs configuration** for macOS, focused on note-taking and writing with Org-mode. It uses a modular approach with configuration split across multiple focused files in the `lisp/` directory.

## Guiding Principles

### 1. Simplicity and Clarity
- **No complexity for its own sake**: Only add features that serve a clear purpose
- **Standard Emacs keybindings**: NO Evil mode, NO Doom-style bindings
- **Prefer built-in solutions**: Use Emacs built-ins when they work well (project.el over projectile)
- **Co-locate related configuration**: Package-specific keybindings stay with their packages

### 2. Modular Organization
- **One concern per file**: Each init file handles a specific domain
- **Clear dependencies**: Files load in order, with explicit `require` statements
- **No global dumping ground**: Utilities are for actual utilities, not miscellaneous config

### 3. Modern Package Management
- **Elpaca** for asynchronous package installation
- **use-package** for declarative configuration
- **NO eval-after-load**: Use proper `:after` dependencies in use-package
- **Consistent patterns**: All packages configured the same way

### 4. Clean File System
- **All temp files in .cache/**: Backups, auto-saves, undo history, databases
- **Single folder cleanup**: Delete `.cache/` to remove all generated files
- **Git-friendly**: .gitignore excludes all generated content

### 5. Typography and Aesthetics
- **Consistent font usage**: Gentium Book Basic for everything (headings and body)
- **Minor Second scale** (1.125 ratio) for heading hierarchy
- **Absolute sizes**: Headings sized absolutely to avoid compounding with body text
- **No unnecessary spacing**: Line-spacing removed for clean appearance

## Configuration Structure

### Entry Point
- **early-init.el**: Pre-package initialization, GC optimization, native-comp settings
- **init.el**: Main entry point that loads all modules in order

### Module Files (lisp/)

All modules follow the naming convention `init-<domain>.el` and provide `'init-<domain>`.

#### Core Infrastructure
- **init-elpaca.el**: Elpaca package manager setup
- **init-defaults.el**: Sensible defaults (UI, file management, editing)
- **init-keybindings.el**: General.el and Which-Key setup ONLY (no package bindings)

#### Appearance
- **init-ui.el**: Themes (Modus with auto-dark), typography, modeline (doom-modeline)
- **init-macos.el**: macOS-specific settings (exec-path-from-shell, key modifiers)

#### Editing and Navigation
- **init-editing.el**: Undo-tree, Avy jump, Jinx spell checking
- **init-completion.el**: Vertico, Orderless, Marginalia, Corfu, Cape, Embark, Consult
- **init-buffers.el**: Ibuffer configuration

#### Tools and Utilities
- **init-utilities.el**: Restart-emacs, Helpful, GCMH, Crux, config helpers
- **init-project.el**: project.el with recursive Git repo discovery
- **init-dired.el**: Dired enhancements (subtree, icons, hide-dotfiles)
- **init-version-control.el**: Magit and magit-delta

#### Note-Taking
- **init-notetaking.el**: Org-mode, Vulpea ecosystem, Deft, capture templates

### Key Variables

**Namespace prefix**: `bs/` for all custom functions and variables

**Configuration variables**:
- `bs/base-font-size`: Base font size in points (default: 14)
- `bs/font-scale-ratio`: Typography scale ratio (1.125 for Minor Second)
- `bs/heading-font`: Font for headings ("Gentium Book Basic")
- `bs/variable-pitch-font`: Font for body text ("Gentium Book Basic")
- `bs/monospace-font`: Font for code ("Lilex Nerd Font")
- `bs/org-directory`: Main org notes directory
- `bs/org-journal-directory`: Journal subdirectory
- `bs/org-books-directory`: Books subdirectory
- `bs/project-search-path`: Root for project discovery

## Package Management Patterns

### use-package Structure
```elisp
(use-package package-name
  :ensure t                    ;; Install via Elpaca
  :demand t                    ;; Load immediately (rare, only for critical packages)
  :after other-package         ;; Wait for dependency
  :hook (mode . function)      ;; Add mode hooks
  :bind (("key" . command))    ;; Keybindings (use sparingly)
  :init                        ;; Code run before package loads
  :config                      ;; Code run after package loads
  (setq package-var value)     ;; Configuration
  (general-define-key ...))    ;; Keybindings (preferred method)
```

### Keybinding Patterns
1. **Use General.el** for all keybindings
2. **Prefix keys must be unbound first**: `(global-unset-key (kbd "M-m"))`
3. **Package bindings in package config**: Don't put them in init-keybindings.el
4. **Which-key descriptions**: Always add for prefix keys

### Prefix Keys Used
- `M-j`: Avy jump commands
- `M-m`: Org/notes commands (vulpea, capture, journal)
- `M-g`: Magit/git commands
- `M-s`: Search commands (deft, consult)
- `C-c c`: Configuration management
- `C-c q`: Quit/restart
- `C-c f`: File operations (crux)
- `C-c b`: Buffer operations (crux)
- `C-c e`: Editing operations (crux)

## Important Decisions and Context

### Typography
- **Font choice**: Gentium Book Basic unifies headings and body for cohesive serif appearance
- **Body text size**: 1.4x base size (19.6pt from 14pt base) for readability
- **Heading sizes**: Calculated as `base-size × scale-ratio^level` for absolute sizing
- **No line spacing**: Removed `line-spacing 0.2` as it created too much whitespace

### Theme
- **Modus Operandi/Vivendi**: Accessible, customizable themes
- **auto-dark**: Automatic switching based on macOS system appearance
- **Theme pre-loading**: Both themes loaded at startup for auto-dark to work
- **magit-delta**: Catppuccin Latte (light) and Mocha (dark) themes

### Note-Taking
- **Vulpea ecosystem**: Standalone database (not org-roam)
- **Directory structure**:
  - Root: Plain notes
  - `journal/`: Daily entries with format "2026-02-03, Tuesday"
  - `books/`: Book notes with AUTHOR field
- **IDs**: All notes get UUID via org-id with uuidgen
- **Property drawers**: Auto-hidden with org-tidy (invisible style)
- **Deft**: Searches same directory as org notes

### Undo Configuration
- **undo-tree**: Persistent undo history in `.cache/undo-tree/`
- **Keybindings**: C-z (undo), C-/ (undo), C-? (redo), M-_ (redo)
- **Note**: C-S-z doesn't work (macOS intercepts it)

### Project Management
- **project.el**: Built-in Emacs 28+ project management
- **Discovery**: Recursive search for .git directories in `~/code`
- **Auto-discovery**: Runs on startup via `emacs-startup-hook`
- **Manual rediscovery**: `C-c c p`

## Common Tasks

### Adding a New Package
1. Add `use-package` declaration in appropriate init file
2. Use `:ensure t` for Elpaca to install
3. Configure with `:config` section
4. Add keybindings with `general-define-key` in `:config`
5. Restart Emacs to install and configure

### Creating a New Module
1. Create `lisp/init-<name>.el`
2. Add file header with `lexical-binding: t`
3. Add `(provide 'init-<name>)` at end
4. Add `(require 'init-<name>)` to init.el in appropriate order
5. Keep concerns focused and single-purpose

### Debugging Configuration
- `M-x restart-emacs`: Restart to reload config
- `C-c c r`: Reload configuration without restart
- `C-c c o`: Open init.el for editing
- Check `*Messages*` buffer for errors
- Use `C-h k` to see what key is bound to
- Use `C-h f` / `C-h v` (helpful) for documentation

### Typography Adjustments
- Change base size: `M-x bs/update-base-font-size`
- Modify `bs/base-font-size` in init-ui.el for permanent change
- Heading scaling happens automatically via `bs/calculate-font-height`
- Body text multiplier in init-notetaking.el (currently 1.4x)

## File System Layout

```
~/.config/emacs/
├── early-init.el           # Pre-package initialization
├── init.el                 # Main entry point
├── .gitignore             # Excludes .cache/, *.db, bookmarks
├── .cache/                # All generated files (gitignored)
│   ├── elpaca/           # Package manager
│   ├── undo-tree/        # Undo history
│   ├── vulpea/           # Note database
│   ├── backups/          # File backups
│   ├── auto-save/        # Auto-save files
│   └── var/              # Misc (savehist, recentf, places)
└── lisp/                  # Configuration modules
    ├── init-elpaca.el
    ├── init-defaults.el
    ├── init-keybindings.el
    ├── init-ui.el
    ├── init-macos.el
    ├── init-editing.el
    ├── init-completion.el
    ├── init-buffers.el
    ├── init-utilities.el
    ├── init-project.el
    ├── init-dired.el
    ├── init-version-control.el
    └── init-notetaking.el
```

## What NOT to Do

### Anti-Patterns to Avoid
- ❌ **Don't use eval-after-load**: Use `:after` in use-package instead
- ❌ **Don't put package bindings in init-keybindings.el**: Keep them with their packages
- ❌ **Don't add line-spacing**: Creates excessive whitespace
- ❌ **Don't use relative heading sizes**: Headings must be absolutely sized
- ❌ **Don't create new files unnecessarily**: ALWAYS prefer editing existing files
- ❌ **Don't use multiple use-package declarations for same package**: Configure once
- ❌ **Don't forget to unbind prefix keys**: M-m, M-j, C-c f, etc. need `global-unset-key`
- ❌ **Don't use with-eval-after-load**: Use proper use-package `:config` or `:after`

### Known Issues
- **C-S-z doesn't work**: macOS intercepts Control+Shift+Z, use C-? or M-_ for redo instead
- **Hardware security keys**: SSH push requires physical key connected
- **vulpea-journal title format**: Must use `vulpea-journal-default-template` plist, not separate variables

## External Dependencies

These tools should be installed via Homebrew for best performance:

- **fd**: Fast directory scanning (15× faster than find)
- **fswatch**: File system change detection for Vulpea auto-sync
- **delta**: Enhanced git diffs (for magit-delta)
- **uuidgen**: UUID generation for org-id (usually pre-installed on macOS)

Install with:
```bash
brew install fd fswatch git-delta
```

## Git Workflow

This configuration uses conventional commits format:

```
<type>(<scope>): <subject>

<body>

Co-Authored-By: Claude Sonnet 4.5 <noreply@anthropic.com>
```

**Types**: feat, fix, refactor, docs, style, test, chore

**Scopes**: phase1-5, ui, editing, completion, notetaking, utilities, etc.

## Future Ideas and Planned Features

The `docs/future-ideas/` directory contains detailed implementation plans for potential features and extensions:

- **tech-radar.md**: Personal Technology Radar system built on Vulpea
  - Track tools, techniques, platforms, languages, and frameworks
  - Query by category, adoption ring, status, and review needs
  - Generate reports and maintain technology decisions over time
  - Estimated implementation: 6-8 hours with Claude Code

When implementing features from this directory:
1. Read the full plan before starting
2. Follow the phased approach outlined in the document
3. Update the plan with any lessons learned
4. Remove from future-ideas or mark as implemented once complete
5. Update CLAUDE.md to document the new capability

## Support and Context

When working with this configuration:
1. Read this file fully for context
2. Check existing patterns before suggesting changes
3. Follow the guiding principles above
4. Test changes before committing
5. Use conventional commit format
6. Keep changes focused and logical

This configuration is actively maintained and reflects deliberate choices made through iterative refinement. Respect the existing patterns and principles unless there's a compelling reason to change them.
