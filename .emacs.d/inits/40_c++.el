;;;
;;; C/C++

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
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
