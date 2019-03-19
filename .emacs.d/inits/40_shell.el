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

(setq sh-basic-offset 2)
(setq sh-indentation 2)

;; fish shell
(use-package fish-mode
  :custom
  (fish-indent-offset 2))
