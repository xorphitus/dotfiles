;;; elisp.el --- Emacs Lisp settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Emacs Lisp settings.

;;; Code:

(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
