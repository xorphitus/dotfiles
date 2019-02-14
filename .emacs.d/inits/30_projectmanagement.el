;;;; projectmanagement.el --- project management settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has project management settings.

;;; Code:

;; projectile
(use-package projectile
  :commands projectile
  ;; helm-projectile
  :bind (("C-c p h" . helm-projectile))
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " 📁 %s" (projectile-project-name)))))

;; eyebrowse
(use-package eyebrowse
  :config
  (eyebrowse-mode t))
