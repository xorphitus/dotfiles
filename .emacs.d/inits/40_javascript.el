;;; javascript.el --- JavaScript settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has JavaScript settings.

;;; Code:

;; js2-mode (forked by id:mooz, fixing identation)
;;  https://raw.github.com/mooz/js2-mode/master/js2-mode.el
(lazyload js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; flycheck
;;  npm: jshint
(add-hook 'js2-mode-hook 'flycheck-mode)

;; swank-js
(slime-setup '(slime-js))

(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))
