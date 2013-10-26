;;; Scala

;; scala-mode
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;; flycheck
(add-hook 'scala-mode-hook 'flycheck-mode)

;; ;; requre ensime
;; ;;  https://github.com/aemoncannon/ensime
;; (add-to-list 'load-path "/usr/share/ensime/elisp")
;; (add-to-list 'exec-path "/usr/share/ensime")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
