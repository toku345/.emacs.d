# .emacs.d

My Emacs configurations.

Modern modular configuration targeting Emacs 30.

## Structure

```
early-init.el        Startup optimization: GC, native-comp, and UI suppression
init.el              load-path setup and module requires only
lisp/
  init-package.el    Package archives and bundled use-package setup
  init-core.el       Core defaults, auto-revert, dired, backups, and history
  init-keys.el       Global key bindings
  init-completion.el Completion stack: Vertico/Consult/Marginalia/Orderless/Embark/Corfu/Cape
  init-ui.el         Theme, modeline, whitespace, line, and symbol highlighting
  init-editing.el    Structural editing, snippets, multiple cursors, undo, and folding
  init-vc.el         magit and diff-hl
  init-project.el    project.el, wgrep, and dumb-jump
  init-lsp.el        eglot, flymake, and tree-sitter via treesit-auto
  init-langs.el      Per-language modes with *-ts-mode and eglot
  init-org.el        org and easy-hugo
  init-misc.el       Miscellaneous utilities
legacy/
  init-legacy-2021.el  Old monolithic configuration kept for reference
```

## Technology Choices

| Area | Choice |
|------|------|
| Package management | `package.el` + bundled `use-package` |
| Minibuffer completion | Vertico + Marginalia + Orderless + Consult + Embark |
| In-buffer completion | Corfu + Cape |
| LSP | built-in eglot |
| Parsing | tree-sitter with `*-ts-mode` + `treesit-auto` |
| Syntax checking | built-in flymake |
| Git | magit + diff-hl |
| Project management | built-in project.el |

## Setup

1. Install Emacs 30 or later, preferably with tree-sitter and native-comp.
2. Place this repository at `~/.emacs.d`.
3. On first startup, `package.el` installs required packages automatically.
   Network access is required.
4. Missing tree-sitter grammars are prompted for on first use because
   `treesit-auto-install` is set to `'prompt`.

### External Tools

- LSP servers: `rust-analyzer` / `gopls` / `pyright` / `typescript-language-server` /
  `solargraph` or `ruby-lsp` / `jdtls` / `terraform-ls`
- Formatters: `prettier` / `rubocop` / `jq`
- Other tools: `multimarkdown` for Markdown / PlantUML jar at `~/bin/plantuml.jar` /
  Roswell for Common Lisp via `~/.roswell/helper.el`

## Key Bindings

| Key | Command |
|------|----------|
| `C-h` | `delete-backward-char`; help is on `C-x ?` |
| `C-t` | `other-window` |
| `C-s` | `consult-line` incremental search |
| `M-s r` | `consult-ripgrep` full-text search |
| `C-.` | `embark-act` |
| `C-c m` | `magit-status` |
| `C-c g n/p/r/s` | diff-hl hunk operations |
| `C-x u` | `vundo` visual undo tree |
