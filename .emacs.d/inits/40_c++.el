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

;; indent
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-toggle-auto-hungry-state 1)
             (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

;; auto spell check by ispell
(add-hook 'c-mode-common-hook
          '(lambda ()
             (flyspell-prog-mode)))
(setq flyspell-issue-welcome-flag nil)

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
