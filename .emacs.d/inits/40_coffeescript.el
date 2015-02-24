;;; coffeescript.el --- CoffeeScript settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has CoffeeScript settings.

;;; Code:

;; coffee-mode
(use-package coffee-mode
  :commands coffee-mode
  :mode (("\\.coffee$" . coffee-mode)
         ("Cakefile"   . coffee-mode))
  :init
  (progn
    (custom-set-variables '(coffee-tab-width 2))
    ;; flycheck
    ;;  npm: coffeelint
    (add-hook 'coffee-mode-hook 'flycheck-mode)

    ;; highlight indentation
    (add-hook 'coffee-mode-hook 'highlight-indentation-mode)))
