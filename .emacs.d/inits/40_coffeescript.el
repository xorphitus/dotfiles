;;;
;;; CoffeeScript

;; cofee-mode
;; https://github.com/defunkt/coffee-mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
    '(lambda()
        (set (make-local-variable 'tab-width) 2)))

;; flycheck
;;  npm: coffeelint
(add-hook 'coffee-mode-hook 'flycheck-mode)
