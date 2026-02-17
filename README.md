# Emacs Configuration

A modern, modular Emacs configuration focused on Org Mode note-taking and writing workflows.

## Features

- **Asynchronous Package Management**: Elpaca for fast, non-blocking package installation
- **Clean File Organization**: All temporary files isolated in `.cache/` directory
- **Modular Design**: Configuration split into focused modules in `lisp/` directory
- **Native Compilation**: Optimized for Emacs 30+ with native-comp support
- **macOS Optimized**: Tailored for macOS with proper PATH and keyboard configuration
- **Writing-Focused**: Typography-first design with variable-pitch fonts and Org Mode enhancements

## Structure

```
~/.config/emacs/
├── early-init.el              # Early initialization, package.el disabled
├── init.el                    # Main entry point
├── CLAUDE.md                  # Configuration documentation for AI assistants
├── lisp/
│   ├── init-elpaca.el         # Package manager setup
│   ├── init-defaults.el       # Sensible defaults
│   ├── init-keybindings.el    # General & Which-Key setup
│   ├── init-ui.el             # Appearance & typography
│   ├── init-macos.el          # macOS-specific settings
│   ├── init-editing.el        # Editing enhancements
│   ├── init-completion.el     # Vertico/Corfu completion stack
│   ├── init-buffers.el        # Buffer management
│   ├── init-utilities.el      # Utility packages (restart-emacs, helpful, gcmh, crux)
│   ├── init-project.el        # Project management (project.el)
│   ├── init-dired.el          # Dired configuration
│   ├── init-version-control.el # Version control (Magit, Magit-Delta)
│   └── init-notetaking.el     # Note-taking (Org Mode, Vulpea, Deft)
└── .cache/                    # All generated files (git-ignored)
```

## Installation

### Prerequisites

**Emacs 30+ with native compilation** (recommended):

```bash
# Using Homebrew on macOS (simplest method)
brew tap d12frosted/emacs-plus
brew install --cask emacs-plus-app

# Verify native compilation is available
emacs --batch --eval "(message \"%s\" (if (and (fboundp 'native-comp-available-p) (native-comp-available-p)) \"Native comp available\" \"No native comp\"))"
```

The `emacs-plus-app` cask includes native compilation and comprehensive image format support by default.

### Setup

```bash
# Clone this repository
git clone https://github.com/YOUR_USERNAME/emacs.d.git ~/.config/emacs

# Start Emacs
emacs

# First launch will take 2-5 minutes as Elpaca downloads and compiles packages
```

## Configuration Philosophy

### Standard Emacs Keybindings

This configuration uses **standard Emacs keybindings** (no Evil mode). Custom prefixes are added for specific workflows:

- `M-m` - Note-taking (Vulpea, org-capture, journal)
- `M-g` - Magit (Git operations)
- `M-j` - Avy (jump navigation)
- `M-s` - Search (Consult commands)
- `C-c f` - File operations (delete, rename, copy)
- `C-c b` - Buffer operations (revert, kill others)
- `C-c e` - Editing shortcuts (duplicate line/region)
- `C-c c` - Configuration management (open, reload)
- `C-c q` - Quit/restart Emacs

### Typography

Uses **Minor Third scale (1.2 ratio)** for harmonious heading sizes:

- **Headings and Body Text**: Literata
- **Code**: FiraCode Nerd Font (monospace)

Base font size is easily configurable:
```elisp
;; Change base font size and all headings will scale automatically
(bs/update-base-font-size 14)  ; or 16, 18, etc.
```

### Clean Separation

All Emacs-generated files (backups, auto-saves, package builds, etc.) are stored in `.cache/` directory:

```bash
# Clean all temporary files
rm -rf ~/.config/emacs/.cache
```

## Key Packages

- **Completion**: Vertico, Orderless, Marginalia, Corfu, Embark, Consult
- **Note-taking**: Org Mode with org-modern, org-superstar, org-appear, org-tidy, Vulpea, Deft
- **Version Control**: Magit, Magit-Delta
- **Utilities**: restart-emacs, Helpful, GCMH, Crux
- **Editing**: Undo-tree, Avy, Jinx (spell-checking)
- **Appearance**: Modus Themes (Operandi), Doom Modeline
- **Project Management**: project.el with auto-discovery

## Note-taking Setup

Notes are stored in iCloud for sync across devices:

```
~/Library/Mobile Documents/com~apple~CloudDocs/ThePlainTextLife/
├── journal/     # Daily journal entries (Vulpea-journal)
├── books/       # Book notes
└── *.org        # General notes
```

- **Vulpea**: Lightweight note management with linking and backlinks
- **Vulpea-journal**: Daily journal with date-stamped entries
- **Deft**: Fast full-text search across all notes
- **org-modern**: Modern styling with keyword prettification enabled

### Key bindings

- `M-m f` - Find note
- `M-m i` - Insert link to note
- `M-m c` - Capture new note
- `M-m j` - Open today's journal
- `M-s d` - Search notes with Deft

## Contributing

For AI assistants (Claude Code, etc.): See `CLAUDE.md` for comprehensive configuration documentation, guiding principles, and common patterns used in this config.

## License

MIT

## Acknowledgments

Built with assistance from [Claude Code](https://claude.ai/code).
