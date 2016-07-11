;;; slime.el --- slime settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has slime settings.

;;; Code:

;; require SBCL
(setq inferior-lisp-program "sbcl")

(use-package slime
  :config
  (slime-setup '(slime-repl slime-fancy slime-banner)))
