;;; haskell.el --- Haskell settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Ruby settings.

;;; Code:

(use-package haskell-mode
  :mode ((".xmobarrc" . haskell-mode))
  :init
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))
