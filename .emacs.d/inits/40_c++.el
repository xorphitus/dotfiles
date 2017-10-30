;;; c++.el --- C/C++ settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has C/C++ settings.

;;; Code:

(defun add-c-hooks (hook)
  "add hook only c-mode-hook, c++-mode-hook. since c-mode-common-hook includes others hooks"
  (add-hook 'c-mode-hook hook)
  (add-hook 'c++-mode-hook hook))

;; TODO: expand-regionの C-, を食ってしまうので修正が必要
;; auto spell check by ispell
;;(add-hook 'c-mode-common-hook
;;          '(lambda ()
;;             (flyspell-prog-mode)))
;;(setq flyspell-issue-welcome-flag nil)

;; flycheck
(add-hook 'c-mode-common-hook 'flycheck-mode)

;; compile C-c
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key mode-specific-map "c" 'compile)))

;; google-c-style
(use-package google-c-style
  :commands google-c-style
  :init
  (progn
    (add-c-hooks 'google-set-c-style)
    (add-c-hooks 'google-make-newline-indent)))

;; GDB
(setq gdb-many-windows t)
;; show value of variable when mouse cursor on
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
;; show I/O buffer
(setq gdb-use-separate-io-buffer t)
