;;; clojure.el --- clojure settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has clojure settings.

;;; Code:

;;; CIDER

(use-package cider
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :config
  (progn
    ;; Hide the *nrepl-connection* and *nrepl-server* buffers
    ;; from appearing in some buffer switching commands like 'C-x b'
    (setq nrepl-hide-special-buffers t)
    ;; The REPL buffer name  will look like cider project-name:port
    (setq nrepl-buffer-name-show-port t)))

;; clj-refactor
(use-package clj-refactor
  :config
  (progn
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c j")))

;;; compojure indentation
(add-hook 'clojure-mode-hook
          (lambda()
            (define-clojure-indent
              (defroutes 'defun)
              (GET 2)
              (POST 2)
              (PUT 2)
              (DELETE 2)
              (HEAD 2)
              (ANY 2)
              (context 2))))

;;; kibit

;; Teach compile the syntax of the kibit output
(use-package compile
  :commands compile
  :init
  (progn
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))))

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

(defun kibit-current-file ()
  "Run kibit on the current file.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile (concat "lein kibit " buffer-file-name)))

;; rainbow delimiters
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

;;; Typed Clojure
(add-hook 'clojure-mode-hook 'typed-clojure-mode)

;;; ClojureScript

;; TODO
;; http://stackoverflow.com/questions/17714106/how-do-i-setup-a-clojurescript-repl-with-emacs
;; https://github.com/cemerick/austin

;;; etc
(add-hook 'clojure-mode-hook 'highlight-indentation-mode)

;; prittify symbols
(my-macro/prettify-symbols
 clojure-mode-hook
 (-concat '(("fn" . ?λ))
          my-const/logical-prettify-symbols-alist
          my-const/relational-prettify-symbols-alist))
