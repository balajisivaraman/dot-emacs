# Quick Additions - Future Capabilities

This document tracks future capabilities that don't require extensive planning but should be added when needed.

## Terminal Emulation: vterm

**Goal**: Replace Alacritty with vterm for integrated terminal experience within Emacs.

**Why**:
- Keep everything in Emacs workflow
- Better integration with Emacs buffers and windows
- Copy/paste between terminal and Emacs seamlessly
- Use Emacs keybindings in terminal

**Package**: `vterm` (requires libvterm and CMake for compilation)

**Prerequisites**:
```bash
brew install cmake libvterm
```

**Implementation Notes**:
- Add `use-package vterm` to appropriate module (maybe `init-utilities.el` or new `init-terminal.el`)
- Consider keybinding like `C-c t` for opening vterm
- May want `vterm-toggle` package for quick toggle behavior
- Configure shell to use (zsh, bash, etc.)
- Set up directory tracking so vterm opens in project directories

**Estimated Time**: 30 minutes - 1 hour (mostly for configuration and keybinding decisions)

---

## AI Integration: Aider or Similar

**Goal**: Integrate AI coding assistance directly within Emacs.

**Options to Evaluate**:
1. **Aider** - AI pair programming in terminal
   - Could run in vterm buffer for integrated experience
   - Already familiar with conventional commits
2. **gptel** - ChatGPT/Claude integration for Emacs
   - Native Emacs package
   - Send regions to LLM, get responses in buffer
3. **ellama** - LLM client for Emacs with ollama backend
   - Local models option
4. **copilot.el** - GitHub Copilot integration
   - Inline completions like VS Code
5. **codeium.el** - Free Copilot alternative

**Implementation Considerations**:
- If using Aider: Run in vterm, maybe with dedicated keybinding
- If using gptel/ellama: Integrate with note-taking workflow, could query notes with AI
- If using copilot/codeium: Configure for code completion in programming modes
- API keys management (use authinfo.gpg or similar for secrets)
- Cost considerations (some are free, some paid)

**Recommended Starting Point**:
- **gptel** for general AI assistance (works with Claude API, OpenAI, etc.)
- Can later add Aider in vterm for focused coding sessions

**Estimated Time**:
- gptel setup: 30 minutes
- Aider in vterm: 15 minutes (after vterm is set up)
- copilot/codeium: 1 hour (more complex auth and configuration)

---

## Notes

These capabilities can be added incrementally as needed:
1. **Start with vterm** - Foundation for better terminal experience
2. **Then add Aider** - Can run in vterm for AI pair programming
3. **Consider gptel** - For quick AI queries without leaving Emacs
4. **Evaluate copilot** - If inline completions would be useful

None of these are complex enough to warrant a detailed implementation plan like Tech Radar. They're mostly package installations with configuration.

---

## When to Implement

Implement when:
- You find yourself switching to Alacritty frequently → Add vterm
- You want AI coding help without leaving Emacs → Add gptel or Aider
- You want inline code completions → Add copilot or codeium

No rush - add them when the need arises.
