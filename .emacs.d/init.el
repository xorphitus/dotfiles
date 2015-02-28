;;; init.el --- xorphitus elisp

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This is an entry point of xorphitus elisp.

;;; Code:

;; gc max memory (128MB)
(setq gc-cons-threshold 134217728)

;; set load path
(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; elpa
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; cask
(require 'cask "~/.cask/cask.el")
(cask-initialize)

;; use-package
(require 'use-package)

;; init loader
(use-package init-loader
   :config
   (progn
     (init-loader-load "~/.emacs.d/inits")
     ;; detect error file
     (defun init-loader-re-load (re dir &optional sort)
       (dolist (el (init-loader--re-load-files re dir sort))
         (condition-case e
             (let* ((lib (locate-library el))
                    (time (-> el f-no-ext load benchmark-run car))
                    (err (error-message-string e)))
               (init-loader-log (s-lex-format "loaded #{lib}. #{time}"))
               (error
                (init-loader-error-log (s-lex-format "#{lib}. #{err}")))))))))
