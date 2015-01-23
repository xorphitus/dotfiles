;;; skk.el --- skk settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has basic key binding settings.

;;; Code:

(--each '("C-o"
          "C-x C-j")
  (global-set-key (kbd it) 'skk-mode))

;;; disable skk-isearch for migemo
(setq skk-isearch-mode-enable nil)
(setq skk-search-start-mode 'latin)
