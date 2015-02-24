;;; defmacros.el --- macros

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has macros.

;;; Code:

(defun add-to-auto-mode-alist (mode file-patterns)
  (mapcar (lambda (file-pattern)
            (add-to-list 'auto-mode-alist (cons file-pattern mode)))
          file-patterns))
