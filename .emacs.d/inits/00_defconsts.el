;;; defconsts.el --- constants

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has constants.

;;; Code:

;;(add-hook 'emacs-lisp-mode-hook
;;          (lambda ()
;;            (push '(">=" . ?≥) prettify-symbols-alist)))

(defconst my-const/lambda-prettify-symbols-alist
  '(("lambda" . ?λ)))

(defconst my-const/relational-prettify-symbols-alist
  '(("!=" . ?≠)
    ("/=" . ?≠)
    (">=" . ?≥)
    ("<=" . ?≤)))

(defconst my-const/logical-prettify-symbols-alist
  '(("and" . ?∧)
    ("or"  . ?∨)))

(defconst my-const/logical-prettify-symbols-ext-alist
  (-concat my-const/lambda-prettify-symbols-alist
           '(("not" . ?¬)
             ("nil" . ?∅))))

(defconst my-const/arror-prettify-symbols-alist
  '(("->" . ?→)
    ("=>" . ?⇒)))
