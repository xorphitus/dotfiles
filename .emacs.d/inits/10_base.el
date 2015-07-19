;;; base.el --- basic settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has basic settings.

;;; Code:

;; show explicit file name
(setq explicit-shell-file-name shell-file-name)

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; file name completion ignore case
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; auto reload buffer which modified by external programs
(global-auto-revert-mode 1)

;; easy to descern buffers of same name files
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; emacs server
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; enable ido
;; to improbe C-x C-f
(ido-mode 1)
(ido-everywhere)
(setq confirm-nonexistent-file-or-buffer nil)

(use-package ido-vertical-mode
  :config
  (progn
    (setq ido-max-window-height 0.75)
    (setq ido-enable-flex-matching t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (ido-vertical-mode 1)))

;; indent
(setq-default indent-tabs-mode nil)

;;; other behaviors

;; change C-a behavior
;; move line head w/o indent, w/ indent
;; http://d.hatena.ne.jp/gifnksm/20100131/1264956220
(defun beginning-of-indented-line (current-point)
  (interactive "d")
  (if (s-match "^[ \t]+$"
               (save-excursion
                 (buffer-substring-no-properties
                  (progn (beginning-of-line) (point))
                  current-point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key "\C-a" 'beginning-of-indented-line)

;; without backup-file
(setq backup-inhibited t)

;; delete auto save files when quit
(setq delete-auto-save-files t)

;; rectanble select
;;  GUI: C-Ret
;;  CUI: 'cua-set-rectangle-mark
;; M-n -> insert numbers incremental
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; specify browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (--first
       (executable-find it)
       '("chromium-browser"
         "google-chrome"
         "google-chrome-stable"
         "google-chrome-beta"
         "firefox")))
