# Copilot Instructions for this Emacs Config

## Build, test, and lint commands

This repository does not define a dedicated `Makefile`, package manager scripts, or test suite (no ERT/Buttercup files are present).

Use Emacs batch checks as the practical validation path:

- Load config in batch mode (smoke check):
  - `emacs -Q --batch -l early-init.el -l init.el --eval "(message \"init loaded\")"`
- Byte-compile local modules:
  - `emacs -Q --batch -L lisp -f batch-byte-compile lisp/bs-core.el lisp/bs-completion.el lisp/bs-writing.el`
- Byte-compile a single file (single-target check):
  - `emacs -Q --batch -L lisp -f batch-byte-compile lisp/bs-writing.el`

## High-level architecture

- Startup is two-phase:
  - `early-init.el`: startup performance/UI defaults (`gc-cons-threshold`, disable `package.el`, disable tool/menu/scroll bars).
  - `init.el`: bootstraps Elpaca, enables `elpaca-use-package-mode`, waits for package queue, then loads local modules.
- Module loading order in `init.el` matters:
  - `bs-core` first (global defaults, key namespace setup, core tooling).
  - `bs-completion` next (Vertico/Orderless/Marginalia/Consult/Embark + Corfu/Cape).
  - `bs-writing` last (Markdown writing UX, Jinx, Tempel, Hugo helpers).
- Local code lives in `lisp/` and is loaded via `(add-to-list 'load-path ...)` + `(require 'bs-...)`.
- Package management is Elpaca + `use-package`; this repo intentionally does not use `package.el` startup flow.

## Key conventions

- Naming:
  - Public custom commands/functions use `bs/` prefix (e.g. `bs/hugo-new-post`).
  - Module files map to provided features (`bs-core.el` provides `bs-core`, etc.).
- Keybinding structure:
  - Prefix namespaces are centralized in `bs-core.el` via `general`: `C-c h` (Hugo), `C-c s` (Search), `C-x p` (Project), `C-c q` (Quit), `C-c e` (Config).
  - Feature modules add bindings under those existing prefixes (e.g. Hugo bindings in `bs-writing.el` under `C-c h`).
- Cache/state hygiene:
  - Runtime artifacts are redirected into `.cache/` under `user-emacs-directory` (`no-littering`, auto-save, backups, savehist/recentf/saveplace, ELN cache, undo-tree history).
- Markdown writing workflow is opinionated:
  - `bs-writing-mode` auto-enables in `markdown-mode` and controls Olivetti + mixed-pitch + visual-line + fringe handling.
  - Hugo helpers assume a project layout with `content/posts/<slug>/index.md` and create an `images/` directory per post.
- Dependency assumptions:
  - `consult-ripgrep` expects `rg` installed.
  - `jinx` expects `enchant`.
  - Icons/modeline expect Nerd Fonts plus `nerd-icons` font install.

## Other assistant config present

- `.claude/settings.local.json` exists and defines local Claude tool permissions (allowed Bash/WebFetch patterns).
- Treat it as operational tooling context; do not assume it defines coding style, architecture, or behavior conventions.
