;;; scheme.el --- Scheme settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Scheme settings.

;;; Code:

(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))
(setq scheme-program-name "gosh -i")

(lazyload scheme-mode "Major mode for Scheme.")
(lazyload run-scheme "Run an inferior Scheme process.")

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window(get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(add-hook 'scheme-mode-hook 'highlight-indentation-mode)

(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
