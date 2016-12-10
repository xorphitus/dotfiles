;;; smartparens.el -- smartparens settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has smartparens settings.

;;; Code:

(use-package smartparens-config
  :commands smartparens-config
  :init
  (progn
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)

    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; keybinding management
    (bind-keys :map sp-keymap
               ("C-M-f" . sp-forward-sexp)
               ("C-M-b" . sp-backward-sexp)

               ("C-M-d" . sp-down-sexp)
               ("C-M-a" . sp-backward-down-sexp)
               ("C-S-a" . sp-beginning-of-sexp)
               ("C-S-d" . sp-end-of-sexp)

               ("C-M-e" . sp-up-sexp)
               ("C-M-u" . sp-backward-up-sexp)
               ("C-M-t" . sp-transpose-sexp)

               ("C-M-n" . sp-next-sexp)
               ("C-M-p" . sp-previous-sexp)

               ("C-M-k" . sp-kill-sexp)
               ("C-M-w" . sp-copy-sexp)

               ("M-<delete>"    . sp-unwrap-sexp)
               ("M-<backspace>" . sp-backward-unwrap-sexp)

               ("C-<right>"   . sp-forward-slurp-sexp)
               ("C-<left>"    . sp-forward-barf-sexp)
               ("C-M-<left>"  . sp-backward-slurp-sexp)
               ("C-M-<right>" . sp-backward-barf-sexp)

               ("M-D"             . sp-splice-sexp)
               ("C-M-<delete>"    . sp-splice-sexp-killing-forward)
               ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
               ("C-S-<backspace>" . sp-splice-sexp-killing-around)

               ("C-]"              . sp-select-next-thing-exchange)
               ("C-<left_bracket>" . sp-select-previous-thing)
               ("C-M-]"            . sp-select-next-thing)

               ("M-F" . sp-forward-symbol)
               ("M-B" . sp-backward-symbol)

               ("H-t"   . sp-prefix-tag-object)
               ("H-p"   . sp-prefix-pair-object)
               ("H-s c" . sp-convolute-sexp)
               ("H-s a" . sp-absorb-sexp)
               ("H-s e" . sp-emit-sexp)
               ("H-s p" . sp-add-to-previous-sexp)
               ("H-s n" . sp-add-to-next-sexp)
               ("H-s j" . sp-join-sexp)
               ("H-s s" . sp-split-sexp)

               ("M-;" . sp-comment))

    (bind-key ")" 'sp-up-sexp emacs-lisp-mode-map)

    ;;;;;;;;;;;;;;;;;;
    ;; pair management
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

    ;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

    ;; tex-mode latex-mode
    (sp-with-modes '(tex-mode plain-tex-mode latex-mode)
      (sp-local-tag "i" "\"<" "\">"))

    ;; html-mode
    (sp-with-modes '(html-mode sgml-mode)
      (sp-local-pair "<" ">"))

    ;; lisp modes
    (sp-with-modes sp--lisp-modes
      ;; disable ', it's the quote character!
      (sp-local-pair "'" nil :actions nil)
      ;; disable `, it's the template character!
      (sp-local-pair "`" nil :actions nil))))
