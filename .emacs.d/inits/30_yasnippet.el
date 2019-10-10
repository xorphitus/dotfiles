;;; yasnippet.el --- yasnippet settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has yasnippet settings.

;;; Code:

(use-package yasnippet
  :diminish (yas-minor-mode . "🅨")
  :config
  (yas-global-mode 1)
  (bind-key "M-=" 'yas-insert-snippet yas-minor-mode-map))
