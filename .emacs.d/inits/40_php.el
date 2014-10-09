;;; php.el --- PHP settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has PHP settings.

;;; Code:

(lazyload php-mode)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(setq php-mode-force-pear t)

(add-hook 'php-mode-hook
          (lambda()
            (flycheck-mode t)))
