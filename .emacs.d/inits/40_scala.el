;;; scala.el --- Scala settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Scala settings.

;;; Code:

;; scala-mode
(use-package scala-mode-auto
  :commands scala-mode-auto
  :mode (("\\.scala$" . scala-mode))
  :init
  (progn
    ;; flycheck
    (add-hook 'scala-mode-hook 'flycheck-mode)))


;; ;; requre ensime
;; ;;  https://github.com/aemoncannon/ensime
;; (add-to-list 'load-path "/usr/share/ensime/elisp")
;; (add-to-list 'exec-path "/usr/share/ensime")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
