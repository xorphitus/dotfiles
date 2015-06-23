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
  (progn
    ;; flycheck
    (add-hook 'go-mode-hook 'flycheck-mode)

    ;; highlight indentation
    (add-hook 'go-mode-hook 'highlight-indentation-mode)

    ;; prittify symbols
    (my-macro/prettify-symbols
     go-mode-hook
     my-const/relational-prettify-symbols-alist)))
