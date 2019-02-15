;;; paren.el -- paren edit settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has paren edit settings.

(use-package paredit
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'common-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode))

;;; Code:
(use-package parinfer
  :bind
  (("C-;" . parinfer-toggle-mode))
  :init
  (progn
    (setq parinfer-extensions
          '(defaults       ; should be included.
            pretty-parens  ; different paren styles for different modes.
            paredit        ; Introduce some paredit commands.
            smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
            smart-yank))   ; Yank behavior depend on mode.
    ;; (add-hook 'clojure-mode-hook #'parinfer-mode)
    ;; (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
    ;; (add-hook 'common-lisp-mode-hook #'parinfer-mode)
    ;; (add-hook 'scheme-mode-hook #'parinfer-mode)
    ;; (add-hook 'lisp-mode-hook #'parinfer-mode)
    ;; modify parinfer mode-line text
    (setq parinfer-lighters '("←λ→" . "（λ）"))))
