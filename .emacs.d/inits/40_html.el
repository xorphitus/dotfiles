;;; HTML
(add-to-list 'auto-mode-alist '("\\.html?" . html-mode))
(add-hook 'html-mode-hook 'flycheck-mode)

;;; Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)

;;; CSS
(add-to-list 'auto-mode-alist '("\\.css" . css-mode))
(add-hook 'css-mode-hook 'flycheck-mode)

;;; LESS
(require 'less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))
(add-hook 'less-css-mode-hook 'flycheck-mode)

;;; Haml
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml" . haml-mode))
(add-hook 'haml-mode-hook 'flycheck-mode)

;;; Sass
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass" . sass-mode))
(add-hook 'sass-mode-hook 'flycheck-mode)

;;; SCSS
(require 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))
(add-hook 'scss-mode-hook 'flycheck-mode)

