;;; defmacros.el --- macros

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has macros.

;;; Code:

(defmacro my-macro/prettify-symbols (hook symbols-alist)
  `(add-hook ',hook
             (lambda ()
               (-each ,symbols-alist
                 (lambda (prettify-map)
                   (push prettify-map prettify-symbols-alist))))))
