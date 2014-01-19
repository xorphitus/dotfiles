;;; Ruby

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))

;; ruby-block
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; flycheck
(add-hook 'ruby-mode-hook 'flycheck-mode)

;; Rinari
;;  https://github.com/eschulte/rinari
(require 'rinari)

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook
          (lambda()
            (ruby-electric-mode t)))

;; bugfix for ruby-electric-space
;; "Symbol's function definition is void: ruby-insert-end"
(defun ruby-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

;; smartparens
;; highlight "class, do, if, ..." on "end".
(add-hook 'ruby-mode-hook 'show-smartparens-mode)
