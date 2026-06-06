# .emacs.d

My Emacs configurations.

Emacs 30 系を前提に、モジュール分割で構成したモダンな設定。

## 構成

```
early-init.el        起動最適化（GC・native-comp・UI 抑制）
init.el              load-path 設定 + 各モジュールの require のみ
lisp/
  init-package.el    パッケージアーカイブ + use-package（同梱）設定
  init-core.el       基本設定（既定値・auto-revert・dired・バックアップ等）
  init-keys.el       グローバルキーバインド
  init-completion.el 補完スタック（Vertico/Consult/Marginalia/Orderless/Embark/Corfu/Cape）
  init-ui.el         テーマ・モードライン・空白表示・行/シンボルハイライト
  init-editing.el    括弧編集・スニペット・マルチカーソル・undo・折り畳み
  init-vc.el         magit・diff-hl
  init-project.el    project.el・wgrep・dumb-jump
  init-lsp.el        eglot・flymake・tree-sitter（treesit-auto）
  init-langs.el      各言語モード（*-ts-mode + eglot）
  init-org.el        org・easy-hugo
  init-misc.el       その他ユーティリティ
legacy/
  init-legacy-2021.el  旧・単一ファイル設定（参照用）
```

## 主要な技術選定（モダン化）

| 領域 | 採用 |
|------|------|
| パッケージ管理 | `package.el` + `use-package`（Emacs 同梱） |
| ミニバッファ補完 | Vertico + Marginalia + Orderless + Consult + Embark |
| バッファ内補完 | Corfu + Cape |
| LSP | eglot（組込） |
| 構文解析 | tree-sitter（`*-ts-mode`） + `treesit-auto` |
| 文法チェック | flymake（組込） |
| Git | magit + diff-hl |
| プロジェクト | project.el（組込） |

## セットアップ

1. Emacs 30 以降をインストール（tree-sitter / native-comp 付きが望ましい）。
2. 本リポジトリを `~/.emacs.d` に配置。
3. 初回起動時に `package.el` が必要なパッケージを自動取得する
   （ネットワーク接続が必要）。
4. tree-sitter 文法は初回利用時に導入を確認される（`treesit-auto-install` が `'prompt`）。

### 外部ツール（言語ごとに必要なもの）

- LSP サーバ: `rust-analyzer` / `gopls` / `pyright` / `typescript-language-server` /
  `solargraph`（または `ruby-lsp`）/ `jdtls` / `terraform-ls` など
- 整形: `prettier` / `rubocop` / `jq`
- その他: `multimarkdown`（Markdown）/ PlantUML jar（`~/bin/plantuml.jar`）/
  Roswell（Common Lisp, `~/.roswell/helper.el`）

## キーバインド（抜粋）

| キー | コマンド |
|------|----------|
| `C-h` | `delete-backward-char`（ヘルプは `C-x ?`） |
| `C-t` | `other-window` |
| `C-s` | `consult-line`（インクリメンタル検索） |
| `M-s r` | `consult-ripgrep`（全文検索） |
| `C-.` | `embark-act` |
| `C-c m` | `magit-status` |
| `C-c g n/p/r/s` | diff-hl ハンク操作 |
| `C-x u` | `vundo`（視覚的 undo ツリー） |
