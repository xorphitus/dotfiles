;;; typescript.el --- TypeScript settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has TypeScript settings.

;;; Code:

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :mode (("\\.ts" . tide-mode)
         ("\\.tsx" . tide-mode))
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))

(use-package typescript
  :mode (("\\.ts" . typescript-mode)
         ("\\.tsx" . typescript-mode)))
