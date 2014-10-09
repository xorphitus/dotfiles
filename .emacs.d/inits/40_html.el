;;; html.el --- HTML, HTML Templates CSS and CSS preprocessors settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has settings for HTML, HTML Templates CSS and CSS preprocessors.

;;; Code:

;;; HTML
(add-to-list 'auto-mode-alist '("\\.html?" . html-mode))
(add-hook 'html-mode-hook 'flycheck-mode)

;;; Emmet
(lazyload emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)

;;; CSS
(add-to-list 'auto-mode-alist '("\\.css" . css-mode))
(add-hook 'css-mode-hook 'flycheck-mode)

;;; LESS
(lazyload less-css-mode)
(add-to-list 'auto-mode-alist '("\\.less" . less-css-mode))
(add-hook 'less-css-mode-hook 'flycheck-mode)

;;; Haml
(lazyload haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml" . haml-mode))
(add-hook 'haml-mode-hook 'flycheck-mode)
(add-hook 'haml-mode-hook 'highlight-indentation-mode)

;;; Slim
(lazyload slim-mode)
(add-to-list 'auto-mode-alist '("\\.slim" . slim-mode))
(add-hook 'slim-mode-hook 'flycheck-mode)
(add-hook 'slim-mode-hook 'highlight-indentation-mode)

;;; Sass
(lazyload sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass" . sass-mode))
(add-hook 'sass-mode-hook 'flycheck-mode)
(add-hook 'sass-mode-hook 'highlight-indentation-mode)

;;; SCSS
(lazyload scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss" . scss-mode))
(add-hook 'scss-mode-hook 'flycheck-mode)

