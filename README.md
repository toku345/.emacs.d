# .emacs.d

My Emacs configurations.

Modern modular configuration targeting Emacs 30.

## Structure

```text
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
  `solargraph` or `ruby-lsp` / `jdtls` / `terraform-ls` / `zls`
- Formatters: `prettier` / `rubocop` / `jq` / `zig fmt`
- Other tools: `multimarkdown` for Markdown / PlantUML jar at `~/bin/plantuml.jar` /
  Roswell for Common Lisp via `~/.roswell/helper.el`

## Development

Run the local validation gate before committing:

```sh
make check
```

The check target runs:

- `git diff --check`
- a coverage check that every `lisp/*.el` module is listed in `ELISP_FILES`
- `emacs -Q --batch -l early-init.el -l init.el`
- `checkdoc` over repository Emacs Lisp files
- ERT tests under `test/` (`emacs -Q --batch -l early-init.el -l init.el
  -l test/init-langs-test.el -f ert-run-tests-batch-and-exit`)
- byte compilation of repository Emacs Lisp files; warnings fail the gate

`emacs` must be available on `PATH`. The first smoke load may access package
archives because this configuration uses `package.el` to install missing
packages automatically.

To delete every `.elc` file in the repository — including compiled files of
installed packages under `elpa/`, which then run uncompiled until
reinstalled or recompiled — run:

```sh
make clean-elc
```

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
