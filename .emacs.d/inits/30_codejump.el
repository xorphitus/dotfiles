;;; codejump.el --- Code Jump (like Ctags) settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has code jump settings.

;;; Code:

;; projectile
(use-package projectile
  :commands projectile
  ;; helm-projectile
  :bind (("C-c p h" . helm-projectile))
  :config
  (projectile-global-mode))

;; dumb-jump
(use-package dumb-jump
  :config
  (dumb-jump-mode))

;; helm
(use-package helm-gtags
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
    (custom-set-variables
     '(helm-gtags-path-style 'relative)
     '(helm-gtags-ignore-case t)
     '(helm-gtags-auto-update t))))
