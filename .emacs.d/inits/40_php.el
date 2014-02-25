;;; PHP
(autoload 'php-mode "php-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(setq php-mode-force-pear t)

(add-hook 'php-mode-hook
          (lambda()
            (flycheck-mode t)))
