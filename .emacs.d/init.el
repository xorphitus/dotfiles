;;; init.el --- xorphitus elisp

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This is an entry point of xorphitus elisp.

;;; Code:

(require 'cl)

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

;; init loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")

;; detect error file
(defun init-loader-re-load (re dir &optional sort)
  (let ((load-path (cons dir load-path)))
    (dolist (el (init-loader--re-load-files re dir sort))
      (condition-case e
          (let ((lib (locate-library el)))
            (let (time (-> el f-no-ext load benchmark-run car))
              (init-loader-log (s-lex-format "loaded #{lib}. #{time}")))
            (error
             (let ((err) (error-message-string e))
               (init-loader-error-log (s-lex-format "#{lib}. #{err}")))))))))

;;; UTF-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
