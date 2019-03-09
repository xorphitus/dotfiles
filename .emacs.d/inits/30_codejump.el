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
  :config
  (setq helm-gtags-path-style 'relative)
  (setq helm-gtags-ignore-case t)
  (setq helm-gtags-auto-update t))

;; smart-jump
(defun my-smart-jump-configuration-with-gtags (modes)
  (progn
    (smart-jump-register :modes modes
                         :jump-fn 'helm-gtags-find-tag)
    (smart-jump-register :modes modes
                         :jump-fn 'xref-find-definitions)))

(use-package smart-jump
  :config
  (smart-jump-setup-default-registers)
  ;; xref config
  ;; eglot uses xref: it means that no special configurations are needed for language servers
  (smart-jump-register :modes '(shell-mode
                                haskell-mode
                                rust-mode))
  ;; xref (lsp) -> gtags config
  (my-smart-jump-configuration-with-gtags '(c-mode-hook
                                            c++-mode-hook
                                            lisp-mode-hook
                                            ruby-mode-hook
                                            js2-mode-hook
                                            python-mode-hook
                                            php-mode-hook)))
