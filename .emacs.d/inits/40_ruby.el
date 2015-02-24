;;; ruby.el --- Ruby settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Ruby settings.

;;; Code:

(add-to-auto-mode-alist 'ruby-mode
                        '("Gemfile"
                          "Rakefile"
                          "Vagrantfile"
                          "Berksfile"
                          "\\.rake"))

;; flycheck
(add-hook 'ruby-mode-hook 'flycheck-mode)

;; Rinari
(use-package rinari)

;; ruby-electric
(use-package ruby-electric
  :init
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

;; smartparens
;; highlight block
(add-hook 'ruby-mode-hook 'show-smartparens-mode)

;; xmpfilter
(use-package rcodetools
  :init
  (define-key ruby-mode-map (kbd "C-c C-d") 'xmp))


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
(add-hook 'robe-mode-hook 'ac-robe-setup)

;; highlight indentation
(add-hook 'ruby-mode-hook 'highlight-indentation-mode)

;; disable magic comment
(defun ruby-mode-set-encoding () nil)
