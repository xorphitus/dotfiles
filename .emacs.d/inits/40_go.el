;;; go.el --- Go Lang settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Go Lang settings.

;;; Code:

;; go-mode
(use-package go-mode
  :commands go-mode
  :init
  ;; flycheck
  (add-hook 'go-mode-hook 'flycheck-mode)
  ;; indentation
  (add-hook 'go-mode-hook 'highlight-indentation-mode)
  (add-hook 'go-mode-hook (lambda ()
                            (setq tab-width 4)))
  ;; prittify symbols
  (my-macro/prettify-symbols
     go-mode-hook
     my-const/relational-prettify-symbols-alist))
