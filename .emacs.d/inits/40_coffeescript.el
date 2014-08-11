;;; coffeescript.el --- CoffeeScript settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has CoffeeScript settings.

;;; Code:

;; coffee-mode
;; https://github.com/defunkt/coffee-mode
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(custom-set-variables '(coffee-tab-width 2))

;; flycheck
;;  npm: coffeelint
(add-hook 'coffee-mode-hook 'flycheck-mode)

;; highlight indentation
(add-hook 'coffee-mode-hook 'highlight-indentation-mode)
