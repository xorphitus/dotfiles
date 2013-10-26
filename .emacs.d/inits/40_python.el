;;;
;;; Python

;; python-mode
;; (python-mode.el)
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook
      '(lambda()
            (setq indent-tabs-mode nil)
            (setq indent-level 4)
            (setq python-indent 4)
            (setq tab-width 4)))

;; flycheck
;;  required package: pylint
(add-hook 'python-mode-hook 'flycheck-mode)
