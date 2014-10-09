;;; yaml.el --- YAML settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has YAML settings.

;;; Code:

(lazyload yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
          '(lambda ()
             (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; flycheck
(add-hook 'yaml-mode-hook 'flycheck-mode)
