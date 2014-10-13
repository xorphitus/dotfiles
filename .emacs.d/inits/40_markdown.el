;;; markdown.el --- Markdown settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Markdown settings.

;;; Code:

(lazyload markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(put 'dired-find-alternate-file 'disabled nil)
