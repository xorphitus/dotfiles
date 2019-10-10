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
  ;;()
  :config
  (setq rustic-rls-pkg 'eglot))


;;;https://github.com/joaotavora/eglot/issues/98
(defun my-project-try-cargo-toml (dir)
  (when-let* ((output
               (let ((default-directory dir))
                 (shell-command-to-string "cargo metadata --no-deps --format-version 1")))
              (js (ignore-errors (json-read-from-string output)))
              (found (cdr (assq 'workspace_root js))))
    (cons 'eglot-project found)))

(cl-defmethod project-roots ((project (head eglot-project)))
  (list (cdr project)))

(add-hook 'project-find-functions 'my-project-try-cargo-toml nil nil)
