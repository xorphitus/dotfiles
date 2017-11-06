;;; ruby.el --- Ruby settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Ruby settings.

;;; Code:

(use-package ruby-mode
  :mode (("Gemfile" . ruby-mode)
          ("Rakefile" . ruby-mode)
          ("Vagrantfile" . ruby-mode)
          ("\\.rake" . ruby-mode))
  :interpreter "ruby"
  :init
  (progn
    ;; highlight indentation
    (add-hook 'ruby-mode-hook 'highlight-indentation-mode))
  :config
  (progn
    ;; flycheck
    (add-hook 'ruby-mode-hook 'flycheck-mode)

    ;; smartparens
    ;; highlight block
    (add-hook 'ruby-mode-hook 'show-smartparens-mode)

    ;; disable magic comment
    (defun ruby-mode-set-encoding () nil)

    ;; prittify symbols
    (my-macro/prettify-symbols
     ruby-mode-hook
     (-concat my-const/lambda-prettify-symbols-alist
           my-const/arror-prettify-symbols-alist
           my-const/relational-prettify-symbols-alist))))

;; Rinari
(use-package rinari)

;; ruby-electric
(use-package ruby-electric
  :diminish ruby-electric-mode
  :config
  (progn
    (add-hook 'ruby-mode-hook
              (lambda()
                (ruby-electric-mode t)))
    ;; bugfix for ruby-electric-space
    ;; "Symbol's function definition is void: ruby-insert-end"
    (defun ruby-insert-end ()
      (interactive)
      (insert "end")
      (ruby-indent-line t)
      (end-of-line))))

;; xmpfilter
(use-package rcodetools
  :init
  (bind-key "C-c C-d" 'xmp ruby-mode-map))

;; inf-ruby
(use-package inf-ruby
  :commands inf-ruby
  :init
  (progn
    (setq inf-ruby-default-implementation "pry")
    (setq inf-ruby-eval-binding "Pry.toplevel_binding")
    (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)))


;; robe
(add-hook 'ruby-mode-hook 'robe-mode)
(use-package company
  :config
  (push 'company-robe company-backends))
