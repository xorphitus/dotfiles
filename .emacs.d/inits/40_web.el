;; http://web-mode.org/
(autoload 'web-mode "web-mode" nil t)
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))

(add-hook 'web-mode-hook 'flycheck-mode)

(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    4))
(add-hook 'web-mode-hook 'web-mode-hook)
