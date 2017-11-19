;;; helm.el --- helm settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has helm settings.

;;; Code:
(use-package helm
  :diminish helm-mode
  :init
  (use-package helm-config
    :bind (("C-c h" . helm-for-files)
           ("M-x"   . helm-M-x)
           ("C-x b" . helm-buffers-list)
           ("M-y"   . helm-show-kill-ring))
    :init
    (progn
      (helm-mode 1)
      ;; http://www49.atwiki.jp/ntemacs/pages/32.html
      ;; helm-for-files can be very heavy
      ;; it's caused by helm-source-files-in-current-dir when using 'tramp'
      (setq helm-for-files-preferred-list
            '(helm-source-buffers-list
              helm-source-bookmarks
              helm-source-recentf
              helm-source-file-cache
              ;; helm-source-files-in-current-dir
              helm-source-locate))
      ;; let C-h backspace in helm
      (bind-key "C-h" 'delete-backward-char helm-map)
      ;; enable helm + migemo
      (use-package migemo
        :diminish (helm-migemo-mode . "🅗🅜")
        :config (helm-migemo-mode 1)))))

;; helm-ag
(use-package helm-ag
  :commands helm-ag
  :bind (("M-g ." . helm-ag)
         ("M-g ," . helm-ag-pop-stack)))
