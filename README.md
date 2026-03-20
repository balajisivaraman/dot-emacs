# Emacs Configuration

Personal Emacs 29+ configuration for Hugo blog writing.

## Requirements

- Emacs 29+
- macOS (works on Linux too; some settings are macOS-specific)
- Fonts:
  - [Inconsolata Nerd Font](https://www.nerdfonts.com/) — monospaced / code
  - [Literata](https://fonts.google.com/specimen/Literata) — variable-pitch prose
  - [Alegreya SC](https://fonts.google.com/specimen/Alegreya+SC) — headings
- `nerd-icons` icon font — run `M-x nerd-icons-install-fonts` on first launch
- [ripgrep](https://github.com/BurntSushi/ripgrep) — required for `consult-ripgrep`
- Optional: `jinx` spell-checker requires `enchant` and a dictionary
  (`brew install enchant` on macOS)

## Installation

```sh
git clone <your-repo-url> ~/.config/emacs
# or symlink an existing clone:
# ln -s ~/path/to/emacs-config ~/.config/emacs
```

Start Emacs. [Elpaca](https://github.com/progfolio/elpaca) bootstraps itself and
installs all packages on the first run — accept any prompts that appear.

After packages install, run `M-x nerd-icons-install-fonts` once to install the icon
font used by the modeline, dired, ibuffer, and completion.

## Structure

```
~/.config/emacs/
├── early-init.el          GC tuning, UI chrome off, package.el disabled
├── init.el                Elpaca bootstrap, load lisp/ modules
├── lisp/
│   ├── bs-core.el         Cache, baseline settings, theme, fonts, tools
│   ├── bs-completion.el   Minibuffer + in-buffer completion stack
│   ├── bs-code-nav.el     Monorepo navigation + optional guarded Eglot
│   └── bs-writing.el      Markdown, writing mode, spell-check, Hugo helpers
└── templates/
    └── tempel             Hugo shortcode snippets
```

## Key Bindings

### Emacs config (`C-c e`)

| Key | Command | Description |
|-----|---------|-------------|
| `C-c e r` | `bs/reload-config` | Reload init.el without restarting |
| `C-c e e` | `bs/open-config` | Open init.el for editing |

### Quit (`C-c q`)

| Key | Command | Description |
|-----|---------|-------------|
| `C-c q q` | `save-buffers-kill-emacs` | Save all and quit |
| `C-c q r` | `restart-emacs` | Restart Emacs |
| `C-x C-c` | _(unbound)_ | Disabled — use `C-c q q` to quit |

### Hugo (`C-c h`)

| Key | Command | Description |
|-----|---------|-------------|
| `C-c h n` | `bs/hugo-new-post` | Create new post (prompts for title) |
| `C-c h r` | `bs/hugo-rename-post` | Rename current post |
| `C-c h p` | `bs/hugo-open-posts-dir` | Open content/posts/ in dired |
| `C-c h d` | `bs/hugo-toggle-draft` | Toggle draft: true/false |

### Search (`C-c s`)

| Key | Command | Description |
|-----|---------|-------------|
| `C-c s b` | `consult-buffer` | Switch buffer with preview |
| `C-c s l` | `consult-line` | Search lines in buffer |
| `C-c s r` | `consult-ripgrep` | Ripgrep across project |
| `C-c s f` | `consult-find` | Find file |
| `C-c s o` | `consult-outline` | Jump to heading |

### Project (`C-x p`)

| Key | Command | Description |
|-----|---------|-------------|
| `C-x p p` | `project-switch-project` | Switch project |
| `C-x p f` | `project-find-file` | Find file in project |
| `C-x p b` | `project-switch-to-buffer` | Switch to project buffer |
| `C-x p k` | `project-kill-buffers` | Kill all project buffers |
| `C-x p g` | `project-find-regexp` | Grep in project |
| `C-x p s` | `bs/consult-ripgrep-project` | Ripgrep from project root |
| `C-x p t` | `bs/vterm-project` | Open terminal in project root |
| `C-x p c` | `bs/copilot-in-project` | Open Copilot terminal in side window |
| `C-x p e` | `bs/reload-project-env` | Reload direnv/mise env for current buffer |
| `C-x p P` | `bs/project-scan-code-dir` | Scan ~/code and register all git projects |
| `C-x p T` | `bs/project-generate-tags` | Generate TAGS with ctags |
| `C-x p V` | `bs/project-visit-tags` | Load project TAGS |
| `C-x p X` | `bs/eglot-maybe` | Start Eglot only if server exists |

### Other

| Key | Command | Description |
|-----|---------|-------------|
| `M-T` | `bs/vterm-scratch` | Open scratch terminal |
| `C-c g` / `C-x g` | `magit-status` | Open Magit |
| `C-x C-j` | `dired-jump` | Jump to dired for current file |
| `C-.` | `embark-act` | Context action menu |
| `M-.` | `embark-dwim` | Smart context action |
| `C-h f` | `helpful-callable` | Describe function |
| `C-h v` | `helpful-variable` | Describe variable |
| `C-h k` | `helpful-key` | Describe key |
| `C-h x` | `helpful-command` | Describe command |
| `C-z` / `C-S-z` | `undo-tree-undo` / `undo-tree-redo` | Undo / redo |
| `M-$` | `jinx-correct` | Correct word at point |
| `C-M-$` | `jinx-correct-all` | Correct all in buffer |
| `M-+` | `tempel-complete` | Expand snippet |
| `C-c t i` | `tempel-insert` | Insert snippet by name |

## Packages

### bs-core.el

| Package | Purpose |
|---------|---------|
| `no-littering` | Keep ~/.config/emacs clean; all state goes to .cache/ |
| `auto-dark` | Auto-switch modus-operandi/vivendi with macOS appearance |
| `nerd-icons` | Icon font support (modeline, dired, ibuffer, completion) |
| `doom-modeline` | Feature-rich modeline with icons |
| `which-key` | Shows key binding continuations in a popup |
| `exec-path-from-shell` | Imports PATH from login shell on macOS |
| `envrc` | Buffer-local project environment loading via direnv |
| `general` | Structured prefix key definition |
| `helpful` | Richer *Help* buffers |
| `transient` | Ensures >=0.12 (magit dependency) |
| `magit` | Git interface |
| `undo-tree` | Visual undo history |
| `ibuffer-vc` | Groups ibuffer entries by version-control root |
| `dired` / `ls-lisp` / `dired-x` | File manager (portable dirs-first, dired-jump) |
| `nerd-icons-dired` | Icons in dired buffers |
| `nerd-icons-ibuffer` | Icons in ibuffer |
| `vterm` | Full terminal emulator |
| `restart-emacs` | Restart without leaving the OS |

### bs-completion.el

| Package | Purpose |
|---------|---------|
| `vertico` | Vertical minibuffer completion list |
| `orderless` | Space-separated fuzzy matching |
| `marginalia` | Annotations (type, docstring) in minibuffer |
| `nerd-icons-completion` | Icons in vertico/marginalia minibuffer |
| `consult` | Rich search/navigation commands |
| `embark` + `embark-consult` | Context-aware actions on any candidate |
| `corfu` | In-buffer completion popup |
| `nerd-icons-corfu` | Icons in corfu popup |
| `cape` | Additional completion sources (file paths, dabbrev) |

### bs-writing.el

| Package | Purpose |
|---------|---------|
| `markdown-mode` | Markdown editing with heading scale, native code fontification |
| `olivetti` | Centred writing column (80 chars) |
| `mixed-pitch` | Variable-pitch prose with fixed-pitch code spans |
| `bs-writing-mode` | Custom minor mode combining olivetti + mixed-pitch + visual-line |
| `jinx` | Fast spell-checker (enchant backend) |
| `tempel` | Lightweight snippet system with Hugo shortcodes |

### bs-code-nav.el

| Command | Purpose |
|---------|---------|
| `bs/project-generate-tags` | Build project-local TAGS for xref-style symbol jumps |
| `bs/project-visit-tags` | Load existing project TAGS |
| `bs/eglot-maybe` | Guarded Eglot startup (quiet fallback when server missing) |

## Monorepo Navigation (Python/JS/React/Terraform/CloudFormation)

Baseline navigation works without Conda/npm/LSP installs:

- `project.el` + `xref` for project-aware navigation
- `consult-ripgrep` for text search across large trees
- TAGS fallback via `C-x p T` and `C-x p V`

## macOS modifier mapping

- Command is mapped to Meta.
- Option remains Alt.

Optional LSP layer:

- Use `C-x p X` (`bs/eglot-maybe`) to start Eglot only when the current mode has a mapped server binary in `PATH`.
- Missing binaries do not error; Emacs stays on xref/consult/tags flow.

## Per-project tools with mise

Use `mise` as the source of truth for project tool versions, and `envrc`/`direnv`
to apply them buffer-locally inside Emacs.

Project setup:

1. Add `.mise.toml` (or `mise.toml`) at project root.
2. Add `.envrc` with:
   `eval "$(mise env --shell zsh)"`
3. Run `direnv allow` in that project root.

In Emacs:

- `envrc-global-mode` is enabled from `bs-core.el`.
- Use `C-x p e` (`bs/reload-project-env`) after changing `.mise.toml` or `.envrc`.

This keeps different projects/buffers on different toolchains (Python/Node/etc.)
without restarting Emacs.

### Optional tooling installs (only if you want LSP)

- Python: `pyright-langserver` (or compatible server)
- JS/TS/React: `typescript-language-server`
- Terraform: `terraform-ls`
- CloudFormation/YAML: `yaml-language-server`

## Copilot Terminal Workflow

- `C-x p c` opens a right-side vterm at the current project root and runs `copilot`.
- `golden-ratio` is enabled so active code windows remain primary while the Copilot terminal stays secondary.

## Writing Workflow

`bs-writing-mode` is a custom minor mode that activates automatically in
`markdown-mode`. It enables `olivetti-mode` (centred 80-column layout),
`mixed-pitch-mode` (variable-pitch prose with monospace code spans), and
`visual-line-mode` for comfortable long-form writing.

Spell-checking is handled by `jinx`, which uses the `enchant` library for fast,
language-aware correction. `M-$` corrects the word at point; `C-M-$` corrects
everything in the buffer.

Snippets for Hugo shortcodes are defined in `templates/tempel`. Expand the snippet
at point with `M-+`, or pick one by name with `C-c t i`.

The Hugo command set (`C-c h`) covers the common post management tasks: creating
posts with auto-generated slugs, renaming posts (updating the filename and front
matter slug together), opening the posts directory, and toggling draft status.

## License

MIT — see [LICENSE](LICENSE).
