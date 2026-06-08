# Repository Guidelines

## Project Structure & Module Organization

This repository is a modular Emacs configuration targeting Emacs 30+. The root
contains `early-init.el` for startup optimization and UI suppression, and
`init.el` for load-path setup plus ordered module loading. Feature-specific
configuration lives in `lisp/init-*.el`; each module should provide the matching
feature name, for example `lisp/init-vc.el` provides `init-vc`. Keep overview
documentation in `README.md`. Runtime state, compiled files, package caches,
`custom.el`, and local history files should stay ignored.

## Build, Test, and Development Commands

- `emacs --batch -l init.el`: smoke-load the full configuration. The first run
  may access package archives to install missing packages.
- `emacs -Q --batch -L lisp -l init-package.el -l lisp/init-core.el`: load a
  small subset when narrowing a module-level issue.
- `git diff --check`: catch whitespace errors before committing.
- `rg "pattern" lisp README.md`: search configuration and docs quickly.

There is no Makefile or dedicated build step; this repository is exercised by
loading Emacs Lisp configuration.

## Coding Style & Naming Conventions

Use Emacs Lisp conventions already present in the repository: `lexical-binding`
headers, `;;; Commentary:`, `;;; Code:`, and matching footer lines. Indent with
spaces, never tabs; the configured default is width 2. Prefer `use-package` for
package configuration and keep built-in packages marked with `:ensure nil`.
Name modules `lisp/init-topic.el`, provide `init-topic`, and add new modules to
`init.el` in dependency order with a short explanatory comment.

## Testing Guidelines

No formal test suite exists yet. For normal changes, run the full smoke-load
command and `git diff --check`. For package, LSP, or language-mode changes, also
open the affected mode interactively when practical. If you add tests, use ERT
under `test/` or `tests/`, name files like `init-topic-test.el`, and document the
exact batch command in `README.md`.

## Commit & Pull Request Guidelines

Git history uses short imperative English subjects such as `Clarify org capture
key binding` and `Avoid startup regressions in package and dired setup`; do not
use Conventional Commit prefixes unless the project adopts them. PRs should
summarize changed modules, mention package or keybinding effects, link related
issues when available, and list the verification commands run.

## Security & Configuration Tips

Do not commit machine-local secrets, cookies, generated package directories, or
personal Custom output. Keep local-only state in ignored files such as
`custom.el`, `var/`, `elpa/`, or history/cache paths covered by `.gitignore`.
