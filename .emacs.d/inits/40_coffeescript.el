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
    (setq coffee-tab-width 2)
    ;; flycheck
    ;;  npm: coffeelint
    (add-hook 'coffee-mode-hook 'flycheck-mode)

    ;; highlight indentation
    (add-hook 'coffee-mode-hook 'highlight-indentation-mode)

    ;; prittify symbols
    (my-macro/prettify-symbols
     coffee-mode-hook
     (-concat my-const/arror-prettify-symbols-alist
              my-const/relational-prettify-symbols-alist))))
