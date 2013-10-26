(setq explicit-shell-file-name shell-file-name)

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; file name completion ignore case
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; auto reload buffer which modified by external programs
(global-auto-revert-mode 1)

;; easy to descern buffers of same name files
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;; styles for coding
(dolist (hook (list
               'text-mode-hook
	       'c-mode-common-hook
	       'emacs-lisp-mode-hook
	       'lisp-interaction-mode-hook
	       'lisp-mode-hook
	       'python-mode-hook
	       'coffee-mode-hook
	       'js2-mode-hook
               'scala-mode-hook
               'ruby-mode-hook
               'haml-mode-hook
               'clojure-mode-hook
	       ))
  (add-hook hook (lambda ()
                   ;; line number
		   (linum-mode t)
		   (set-face-attribute 'linum nil :foreground "#f00" :height 0.9)
		   (setq linum-format "%4d.")
                   ;; clarify whitespace at line tails
                   (setq show-trailing-whitespace t)
                   (set-face-background 'trailing-whitespace "#ff0")
		   )))

;; indent
(setq-default indent-tabs-mode nil)

;;; other behaviors

;; change C-a behavior
;; move line head w/o indent, w/ indent
;; http://d.hatena.ne.jp/gifnksm/20100131/1264956220
(defun beginning-of-indented-line (current-point)
  (interactive "d")
  (if (string-match
       "^[ \t]+$"
       (save-excursion
	 (buffer-substring-no-properties
	  (progn (beginning-of-line) (point))
	  current-point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key "\C-a" 'beginning-of-indented-line)

;; without backup-file
(setq backup-inhibited t)

;; delete auto save files when quit
(setq delete-auto-save-files t)

;; rectanble select
;;  GUI: C-Ret
;;  CUI: 'cua-set-rectangle-mark
;; M-n -> insert numbers incremental
(cua-mode t)
(setq cua-enable-cua-keys nil)

;;; UTF-8
(set-default-coding-systems 'utf-8)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)

;; Japanese input

;; mozc
(when (eq system-type 'gnu/linux)
  (require 'mozc)
  (set-language-environment "Japanese")
  (setq default-input-method "japanese-mozc")
  (global-set-key (kbd "C-o") 'toggle-input-method))
