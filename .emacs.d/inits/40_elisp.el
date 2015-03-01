;;; elisp.el --- Emacs Lisp settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Emacs Lisp settings.

;;; Code:

(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)

(add-hook 'emacs-lisp-mode-hook 'highlight-indentation-mode)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(defconst lisp--prettify-symbols-alist
  (-concat my-const/lambda-prettify-symbols-alist
           my-const/logical-prettify-symbols-alist
           my-const/relational-prettify-symbols-alist))
