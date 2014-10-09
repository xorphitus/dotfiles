;;; defmacros.el --- macros

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has macros.

;;; Code:

(defmacro lazyload (name &rest body)
  `(autoload ',name ,(symbol-name name) nil t))
