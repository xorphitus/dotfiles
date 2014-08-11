;;; erlang.el --- Erlang settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Erlang settings.

;;; Code:

;; define env ERLANG_HOME
;; TODO fix it by using s.el and f.el
(cond ((file-exists-p (concat (getenv "ERLANG_HOME") "/lib/tools-2.6.6.1/emacs"))
    (setq load-path (cons  (concat (getenv "ERLANG_HOME") "/lib/tools-2.6.6.1/emacs") load-path))
    (setq erlang-root-dir (getenv "ERLANG_HOME"))
    (setq exec-path (cons (concat (getenv "ERLANG_HOME") "/bin") exec-path))
    (require 'erlang-start)))
