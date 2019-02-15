;;; codejump.el --- Code Jump (like Ctags) settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has code jump settings.

;;; Code:

;; dumb-jump
(use-package dumb-jump
  :config
  (dumb-jump-mode))

;; helm
(use-package helm-gtags
  :bind (("M-t" . helm-gtags-find-tag)
         ("M-," . helm-gtags-pop-stack))
  :config
  (progn
    ;; hooks
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'lisp-mode-hook 'helm-gtags-mode)
    (add-hook 'ruby-mode-hook 'helm-gtags-mode)
    (add-hook 'js2-mode-hook 'helm-gtags-mode)
    (add-hook 'python-mode-hook 'helm-gtags-mode)
    (add-hook 'php-mode-hook 'helm-gtags-mode)
    ;; customize
    (setq helm-gtags-path-style 'relative)
    (setq helm-gtags-ignore-case t)
    (setq helm-gtags-auto-update t)))
