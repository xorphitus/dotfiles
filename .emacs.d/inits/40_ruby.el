;;; Ruby

;; ruby-block.el
(require 'ruby-block)
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; flycheck
(add-hook 'ruby-mode-hook 'flycheck-mode)

;; Rinari
;;  https://github.com/eschulte/rinari
(require 'rinari)
