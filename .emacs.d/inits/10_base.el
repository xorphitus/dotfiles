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
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

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
