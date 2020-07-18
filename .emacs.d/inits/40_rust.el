;;; rust.el --- Rust settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Rust settings.
;;
;; It requires some Rust components
;;
;; $ rustup component add rls
;; $ rustup component add clippy
;; $ rustup component add rustfmt
;; $ rustup component add rust-src

;;; Code:

(use-package rustic
  :commands
  (rustic-mode)
  :mode
  (("\\.rs\\'" . rustic-mode))
  :config
  (setq rustic-lsp-client 'eglot)
  (add-to-list 'eglot-server-programs '(rustic-mode . ("rust-analyzer")))
  (setq rustic-lsp-server 'rust-analyzer)
  (push 'rustic-clippy flycheck-checkers))
