;;;
;;; Erlang

;; define env ERLANG_HOME
(cond ((file-exists-p (concat (getenv "ERLANG_HOME") "/lib/tools-2.6.6.1/emacs"))
    (setq load-path (cons  (concat (getenv "ERLANG_HOME") "/lib/tools-2.6.6.1/emacs") load-path))
    (setq erlang-root-dir (getenv "ERLANG_HOME"))
    (setq exec-path (cons (concat (getenv "ERLANG_HOME") "/bin") exec-path))
    (require 'erlang-start)
))
