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
    (let ((my-skk-jisyo-root "/usr/share/skk"))
      (setq skk-large-jisyo (concat my-skk-jisyo-root "/SKK-JISYO.L"))
      (setq skk-extra-jisyo-file-list
            (--map (concat my-skk-jisyo-root it)
                   '("/SKK-JISYO.geo"
                     "/SKK-JISYO.jinmei"
                     "/SKK-JISYO.propernoun"
                     "/SKK-JISYO.station"))))))
