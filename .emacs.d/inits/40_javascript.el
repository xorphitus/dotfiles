;;;
;;; JavaScript

;; js2-mode (forked by id:mooz, fixing identation)
;;  https://raw.github.com/mooz/js2-mode/master/js2-mode.el
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; flycheck
;;  npm: jshint
(add-hook 'js2-mode-hook 'flycheck-mode)

;; swank-js
(require 'slime)
(slime-setup '(slime-repl slime-fancy slime-banner slime-js))

(global-set-key [f5] 'slime-js-reload)
(add-hook 'js2-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))
