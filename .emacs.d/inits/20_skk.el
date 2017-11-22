;;; skk.el --- skk settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has basic key binding settings.

;;; Code:

(defcustom my-skk-jisyo-root "/usr/share/skk"
  "SKK dictionary path. Override it for each platform")

(use-package skk
  :bind (("C-o"     . skk-mode)
         ("C-x C-j" . skk-mode))
  :config
  (progn
    ;; enable AZIK
    (setq skk-use-azik t)
    ;; disable skk-isearch for migemo
    (setq skk-isearch-mode-enable nil)
    (setq skk-search-start-mode 'latin)
    ;; candidates position
    (setq skk-show-tooltip t)
    ;; dynamic completion
    ;;   てん -> てんさい, てんしょく、てんかん
    (setq skk-dcomp-activate t)
    (setq skk-dcomp-multiple-activate t)
    (setq skk-dcomp-multiple-rows 10)
    ;; face setting
    ;; it depends on popup.el and its face settings
    (use-package popup
      :config
      (setq skk-tooltip-function
            #'(lambda (tooltip-str)
                (popup-tip tooltip-str))))
    (set-face-foreground 'skk-dcomp-multiple-face (face-attribute 'popup-face :foreground))
    (set-face-background 'skk-dcomp-multiple-face (face-attribute 'popup-face :background))
    (set-face-foreground 'skk-dcomp-multiple-selected-face (face-attribute 'popup-tip-face :foreground))
    (set-face-background 'skk-dcomp-multiple-selected-face (face-attribute 'popup-tip-face :background))
    ;; dictionary
    (setq skk-large-jisyo (concat my-skk-jisyo-root "/SKK-JISYO.L"))
    (setq skk-extra-jisyo-file-list
          (--map (concat my-skk-jisyo-root it)
                 '("/SKK-JISYO.geo"
                   "/SKK-JISYO.jinmei"
                   "/SKK-JISYO.propernoun"
                   "/SKK-JISYO.station")))))
