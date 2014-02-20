;;; HTML
(add-to-list 'auto-mode-alist '("\\.html?" . html-mode))
(add-hook 'html-mode-hook 'flycheck-mode)

;;; Emmet
(autoload 'emmet-mode "emmet-mode" nil t)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;;; CSS
(add-to-list 'auto-mode-alist '("\\.css" . css-mode))
(add-hook 'css-mode-hook 'flycheck-mode)

;;; LESS
(autoload 'less-css-mode "less-css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))
(add-hook 'less-css-mode-hook 'flycheck-mode)

;;; Haml
(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml" . haml-mode))
(add-hook 'haml-mode-hook 'flycheck-mode)
(add-hook 'haml-mode-hook 'highlight-indentation-mode)

;;; Slim
(autoload 'slim-mode "slim-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.slim" . slim-mode))
(add-hook 'slim-mode-hook 'flycheck-mode)
(add-hook 'slim-mode-hook 'highlight-indentation-mode)

;;; Sass
(autoload 'sass-mode "sass-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sass" . sass-mode))
(add-hook 'sass-mode-hook 'flycheck-mode)
(add-hook 'sass-mode-hook 'highlight-indentation-mode)

;;; SCSS
(autoload 'scss-mode "scss-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))
(add-hook 'scss-mode-hook 'flycheck-mode)

