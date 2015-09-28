;;; javascript.el --- JavaScript settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has JavaScript settings.

;;; Code:

;; js2-mode
(use-package js2-mode
  :commands js2-mode
  :mode (("\\.js$" . js2-mode))
  :init
  (progn
    (setq js2-basic-offset 2)

    ;; flycheck
    ;;  npm: eslint
    (add-hook 'js2-mode-hook 'flycheck-mode)
    ;; disable jshint for eslint
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))


    (global-set-key [f5] 'slime-js-reload)
    (add-hook 'js2-mode-hook
              (lambda ()
                (slime-js-minor-mode 1)))

    ;; prettify symbols
    (my-macro/prettify-symbols
     js2-mode-hook
     (-concat '(("function" . ?ƒ))
              my-const/arror-prettify-symbols-alist
              my-const/relational-prettify-symbols-alist))))
