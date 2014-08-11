;;; el-get.el --- el-get settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has settings for el-get.

;;; Code:

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync '(
                navi2ch
                ddskk
                ;; howm
                ))