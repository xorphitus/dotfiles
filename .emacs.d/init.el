;;; init.el --- xorphitus elisp

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This is an entry point of xorphitus elisp.

;;; Code:

;; gc max memory (128MB)
(setq gc-cons-threshold 134217728)

;; hide basic gui widgets first
;; I don't want to show them in initialing
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)

;; set load path
(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; elpa
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu". "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; use-package
(require 'use-package)

;; ensure pacakges
(load (locate-user-emacs-file "my_packages"))

;; init loader
(use-package init-loader
   :config
   (progn
     (init-loader-load "~/.emacs.d/inits")
     ;; detect error file
     (defun init-loader-re-load (re dir &optional sort)
       (dolist (el (init-loader--re-load-files re dir sort))
         (condition-case e
             (let* ((lib (locate-library el))
                    (time (-> el f-no-ext load benchmark-run car))
                    (err (error-message-string e)))
               (init-loader-log (s-lex-format "loaded #{lib}. #{time}"))
               (error
                (init-loader-error-log (s-lex-format "#{lib}. #{err}")))))))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style (quote relative))
 '(package-selected-packages
   (quote
    (yasnippet wrap-region wgrep visual-regexp-steroids undo-tree smartrep shell-pop quickrun protobuf-mode open-junk-file neotree multiple-cursors migemo ido-vertical-mode helm-swoop helm-perspeen helm-ghq helm-ag expand-region esup direnv ddskk anzu ace-window ace-isearch helm-gtags dumb-jump helm-projectile projectile volatile-highlights rainbow-delimiters highlight-indentation spaceline spacemacs-theme diminish git-timemachine git-gutter-fringe gitconfig-mode gitignore-mode magit dockerfile-mode fish-mode php-mode markdown-mode web-mode emmet-mode google-c-style scala-mode rust-mode yaml-mode sass-mode slim-mode scss-mode haml-mode robe ruby-electric rinari vue-mode tide js2-mode coffee-mode haskell-mode go-mode slime-company slime clojure-mode clojure-cheatsheet clj-refactor cider parinfer use-package init-loader drag-stuff cask)))
 '(spacemacs-theme-custom-colors (quote ((base . "#dddddd"))))
 '(yas-prompt-functions (quote (my-yas/prompt))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
