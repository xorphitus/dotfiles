;;; shell.el --- Shell Script settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Shell Script settings.

;;; Code:

;; for arch linux
(add-to-auto-mode-alist 'shell-script-mode '("PKGBUILD"
                                             "\\.install$"))
