;;; defmacros.el --- macros

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has macros.

;;; Code:

(defmacro lazyload (name &rest body)
  `(autoload ',name ,(symbol-name name) nil t))

(defun add-to-auto-mode-alist (mode file-patterns)
  (mapcar (lambda (file-pattern)
            (add-to-list 'auto-mode-alist (cons file-pattern mode)))
          file-patterns))
