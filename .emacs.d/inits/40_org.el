;;; org.el --- constants

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has org-mode settings.

;;; Code:

(setq org-agenda-files (list "~/Documents/org"))

(set-face-attribute 'org-level-1 nil :height 1.4)

;; beautify org-mode list bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; pomodoro
(use-package org-pomodoro
  :after
  org-agenda
  :custom
  (org-pomodoro-format "🍅%s")
  (org-pomodoro-short-break-format "☕%s")
  (org-pomodoro-long-break-format  "🌴%s"))
