;;; rust.el --- Rust settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Rust settings.

;;; Code:

(use-package rust-mode
  :commands rust-mode
  :mode (("\\.rs\\'" . rust-mode)))
