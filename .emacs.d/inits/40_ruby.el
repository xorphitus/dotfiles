;;; Ruby

(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))

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
;; highlight block
(add-hook 'ruby-mode-hook 'show-smartparens-mode)

;; xmpfilter
(require 'rcodetools)
(define-key ruby-mode-map (kbd "C-c C-d") 'xmp)

;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; highlight indentation
(add-hook 'ruby-mode-hook 'highlight-indentation-mode)
