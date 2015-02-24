;;; helm.el --- helm settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has helm settings.

;;; Code:
(use-package helm-comfig
  :bind (("C-c h" . helm-for-files)
         ("C-x b" . helm-buffers-list)
         ("M-y"   . helm-show-kill-ring))
  :init
  (progn
    (helm-mode 1)
    ;; disable helm-find-file
    (add-to-list 'helm-completing-read-handlers-alist '(find-file . nil))
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
    (define-key helm-map (kbd "C-h") 'delete-backward-char)))

;; helm-migemo
(use-package helm-migemo
  :config
  ;; http://rubikitch.com/2014/12/19/helm-migemo/
  (defun helm-compile-source--candidates-in-buffer (source)
    (helm-aif (assoc 'candidates-in-buffer source)
        (append source
                `((candidates
                   . ,(or (cdr it)
                          (lambda ()
                            ;; Do not use `source' because other plugins
                            ;; (such as helm-migemo) may change it
                            (helm-candidates-in-buffer (helm-get-current-source)))))
                  (volatile) (match identity)))
      source)))

;; helm-ag
(use-package helm-ag
  :commands helm-ag
  :bind (("M-g ." . helm-ag)
         ("M-g ," . helm-ag-pop-stack)))

;; helm-projectile
(global-set-key (kbd "C-c p h") 'helm-projectile)
