;;; paren.el -- paren edit settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has paren edit settings.

;;; Code:

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'common-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode))
