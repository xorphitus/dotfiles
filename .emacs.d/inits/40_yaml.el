;;; yaml.el --- YAML settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has YAML settings.

;;; Code:

(use-package yaml-mode
  :commands yaml-mode
  :mode (("\\.yml$" . yaml-mode))
  :init
  (progn
    (add-hook 'yaml-mode-hook
              '(lambda ()
                 (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

    ;; flycheck
    (add-hook 'yaml-mode-hook 'flycheck-mode)))
