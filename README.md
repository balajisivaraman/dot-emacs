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
├── early-init.el          # Early initialization, package.el disabled
├── init.el                # Main entry point
├── lisp/
│   ├── init-elpaca.el     # Package manager setup
│   ├── init-defaults.el   # Sensible defaults
│   ├── init-ui.el         # Appearance & typography
│   ├── init-macos.el      # macOS-specific settings
│   ├── init-editing.el    # Editing enhancements
│   ├── init-completion.el # Vertico/Corfu completion stack
│   ├── init-buffers.el    # Buffer management
│   ├── init-dired.el      # Dired configuration
│   ├── init-org.el        # Org Mode & Vulpea
│   ├── init-tools.el      # Magit, Deft, etc.
│   └── init-keybindings.el # Global keybindings
└── .cache/                # All generated files (git-ignored)
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

- `M-m` - Org Mode & note-taking
- `M-g` - Magit (Git operations)
- `M-j` - Avy (jump navigation)
- `M-s` - Search (Consult commands)

### Typography

Uses **Minor Third scale (1.2 ratio)** for harmonious heading sizes:

- **Headings**: Vollkorn SC (Small Caps, Bold)
- **Body Text**: SN Pro (variable-pitch)
- **Code**: Lilex Nerd Font (monospace)

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
- **Org Mode**: org-modern, org-superstar, org-appear, Vulpea
- **Tools**: Magit, Deft, Helpful
- **Editing**: Undo-tree, Avy, Jinx
- **Themes**: Modus Themes (Operandi)
- **Modeline**: Doom Modeline

## Org Mode Setup

Org files are stored in iCloud:

```
~/Library/Mobile Documents/com~apple~CloudDocs/ThePlainTextLife/
```

- **Vulpea**: Lightweight note-taking and linking
- **Deft**: Fast text search across notes
- **Journal**: Dedicated journal entries in `journal/` subdirectory

## License

MIT

## Acknowledgments

Built with assistance from [Claude Code](https://claude.ai/code).
