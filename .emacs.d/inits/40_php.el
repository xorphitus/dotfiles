;;; php.el --- PHP settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has PHP settings.

;;; Code:

(use-package php-mode
  :commands php-mode
  :mode (("\\.php$" . php-mode))
  :init
  (progn
    (setq php-mode-force-pear t)
    (add-hook 'php-mode-hook
              (lambda ()
                (flycheck-mode t)
                (setq tab-width 4)
                (setq c-basic-offset 4)))))
