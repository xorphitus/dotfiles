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
(use-package emmet-mode
  :commands emmet-mode
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)))

;;; CSS
(add-to-list 'auto-mode-alist '("\\.css" . css-mode))
(add-hook 'css-mode-hook 'flycheck-mode)

;;; LESS
(use-package less-css-mode
  :commands less-css-mode
  :mode (("\\.less" . less-css-mode))
  :init
  (add-hook 'less-css-mode-hook 'flycheck-mode))

;;; Haml
(use-package haml-mode
  :commands haml-mode
  :mode (("\\.haml" . haml-mode))
  :init
  (progn
    (add-hook 'haml-mode-hook 'flycheck-mode)
    (add-hook 'haml-mode-hook 'highlight-indentation-mode)))

;;; Slim
(use-package slim-mode
  :commands slim-mode
  :mode (("\\.slim" . slim-mode))
  :init
  (progn
    (add-hook 'slim-mode-hook 'flycheck-mode)
    (add-hook 'slim-mode-hook 'highlight-indentation-mode)))

(setq css-indent-offset 2)

;;; Sass
(use-package sass-mode
  :commands sass-mode
  :mode (("\\.sass" . sass-mode))
  :init
  (progn
    (add-hook 'sass-mode-hook 'flycheck-mode)
    (add-hook 'sass-mode-hook 'highlight-indentation-mode)))

;;; SCSS
(use-package scss-mode
  :commands scss-mode
  :mode (("\\.scss" . scss-mode))
  :init
  (progn
    (add-hook 'scss-mode-hook 'flycheck-mode)))
