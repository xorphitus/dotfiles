(mapc
 (lambda (hook)
   (add-hook hook 'paredit-mode))
 '(emacs-lisp-mode-hook
   lisp-interaction-mode-hook
   lisp-mode-hook
   clojure-mode-hook
   cider-repl-mode-hook
   scheme-mode-hook))
