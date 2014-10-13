;;; coffeescript.el --- CoffeeScript settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has CoffeeScript settings.

;;; Code:

;; coffee-mode
;; https://github.com/defunkt/coffee-mode
(lazyload coffee-mode)
(add-to-auto-mode-alist 'coffee-mode '("\\.coffee$"
                                       "Cakefile"))
(custom-set-variables '(coffee-tab-width 2))

;; flycheck
;;  npm: coffeelint
(add-hook 'coffee-mode-hook 'flycheck-mode)

;; highlight indentation
(add-hook 'coffee-mode-hook 'highlight-indentation-mode)
