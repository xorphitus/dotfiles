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
    (setq skk-search-start-mode 'latin)
    (setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L")
    (setq skk-extra-jisyo-file-list
          (list '("/usr/share/skk/SKK-JISYO.geo"
                  "/usr/share/skk/SKK-JISYO.jinmei"
                  "/usr/share/skk/SKK-JISYO.propernoun"
                  "/usr/share/skk/SKK-JISYO.station")))))
