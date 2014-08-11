;;; erlang.el --- Erlang settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Erlang settings.

;;; Code:

;; define env ERLANG_HOME
(let* ((erlang-home (getenv "ERLANG_HOME"))
       (erlang-emacs (car (f-glob
                           (concat erlang-home "/lib/tools-*/emacs")))))
  (cond ((f-exists? erlang-emacs)
         (setq load-path (cons erlang-emacs load-path))
         (setq erlang-root-dir erlang-home)
         (setq exec-path (cons (concat erlang-home "/bin") exec-path))
         (require 'erlang-start))))
