;;; CIDER
(add-hook 'clojure-mode-hook 'cider-mode)

;; Enable eldoc in Clojure buffers
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Hide the *nrepl-connection* and *nrepl-server* buffers
;; from appearing in some buffer switching commands like 'C-x b'
(setq nrepl-hide-special-buffers t)

;; ;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
;;(setq cider-repl-pop-to-buffer-on-connect nil)

;; ;; To auto-select the error buffer when it's displayed
;; (setq cider-auto-select-error-buffer t)

;; The REPL buffer name  will look like cider project-name:port
(setq nrepl-buffer-name-show-port t)

;; ;; Make 'C-c C-z' switch to the CIDER REPL buffer in the current window
;; (setq cider-repl-display-in-current-window t)

;; ;; To make the REPL history wrap around when its end is reached
;; (setq cider-repl-wrap-history t)

;;; ac-nrepl
(autoload 'ac-nrepl "ac-nrepl" nil t)
(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

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

;;; ClojureScript
(add-to-list 'auto-mode-alist '("\\.cljs" . clojurescript-mode))

;; TODO
;; http://stackoverflow.com/questions/17714106/how-do-i-setup-a-clojurescript-repl-with-emacs
;; https://github.com/cemerick/austin
