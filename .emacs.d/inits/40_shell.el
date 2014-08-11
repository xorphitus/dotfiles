;;; shell.el --- Shell Script settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Shell Script settings.

;;; Code:

;; for arch linux
(add-to-list 'auto-mode-alist '("PKGBUILD" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . shell-script-mode))
