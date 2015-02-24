;;; clojure.el --- clojure settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has clojure settings.

;;; Code:

;;; CIDER
(add-hook 'clojure-mode-hook 'cider-mode)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide the *nrepl-connection* and *nrepl-server* buffers
;; from appearing in some buffer switching commands like 'C-x b'
(setq nrepl-hide-special-buffers t)

;; The REPL buffer name  will look like cider project-name:port
(setq nrepl-buffer-name-show-port t)

;;; ac-cider
(use-package ac-cider
  :commands ac-cider
  :init
  (progn
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)
    (eval-after-load "auto-complete"
      '(progn
         (add-to-list 'ac-modes 'cider-mode)
         (add-to-list 'ac-modes 'cider-repl-mode)))))

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
