;;; encoding.el --- xorphitus elisp

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file character encoding settings.

;;; Code:

;; UTF-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
