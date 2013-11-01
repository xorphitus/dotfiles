;;; PHP
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(setq php-mode-force-pear t)

(add-hook 'php-mode-hook 'flycheck-mode)
