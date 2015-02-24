;;; shell.el --- Shell Script settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Shell Script settings.

;;; Code:

;; for arch linux
(use-package shell-script-mode
  :mode (("PKGBUILD" . shell-script-mode)
         ("\\.install$" . shell-script-mode)))
