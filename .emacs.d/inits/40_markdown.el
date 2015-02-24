;;; markdown.el --- Markdown settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Markdown settings.

;;; Code:

(use-package markdown-mode
  :commands markdown-mode
  :mode (("\\.md$" . markdown-mode))
  :init
  (put 'dired-find-alternate-file 'disabled nil))
