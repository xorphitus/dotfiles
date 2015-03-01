;;; skk.el --- skk settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has basic key binding settings.

;;; Code:

(use-package skk
  :bind (("C-o"     . skk-mode)
         ("C-x C-j" . skk-mode))
  :config
  (progn
    ;; disable skk-isearch for migemo
    (setq skk-isearch-mode-enable nil)
    (setq skk-search-start-mode 'latin)))
