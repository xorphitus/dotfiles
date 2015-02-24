;;; web.el --- web-mode settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has web-mode settings.

;;; Code:

;; http://web-mode.org/
(use-package web-mode
  :commands web-mode
  :mode (("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode)
         ("\\.erb\\'"      . web-mode)
         ("\\.mustache\\'" . web-mode))
  :init
  (progn
    (add-hook 'web-mode-hook 'flycheck-mode)

    (defun web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-html-offset   2)
      (setq web-mode-css-offset    2)
      (setq web-mode-script-offset 2)
      (setq web-mode-php-offset    4))
    (add-hook 'web-mode-hook 'web-mode-hook)))

