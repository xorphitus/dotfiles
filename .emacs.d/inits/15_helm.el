;;; helm.el --- helm settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has helm settings.

;;; Code:

(require 'helm-config)
(helm-mode 1)

;; disable helm-find-file
(add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))

(define-key global-map (kbd "C-c h") 'helm-for-files)
(define-key global-map (kbd "C-x b") 'helm-buffers-list)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)

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
;; http://mikio.github.io/article/2013/01/31_helmc-h.html
(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-h") 'delete-backward-char)))

;; helm-ag
;; http://d.hatena.ne.jp/syohex/20130302/1362182193
(autoload 'helm-ag "helm-ag" nil t)
(global-set-key (kbd "M-g .") 'helm-ag)
(global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)

;; helm-projectile
(global-set-key (kbd "C-c p h") 'helm-projectile)
