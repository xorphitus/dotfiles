;;; org.el --- constants

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has org-mode settings.

;;; Code:

(setq org-agenda-files (list "~/Documents/org"))

;; beautify org-mode list bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
