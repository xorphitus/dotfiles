;;;; projectmanagement.el --- project management settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has project management settings.

;;; Code:

;; rename workspace name automaticaly!
(defun my-auto-set-projectile-root-to-eyebrowse ()
  (ignore-errors
    (let ((current-root "TODO: get from eyebrowse")
          (projectile-root (-> (projectile-project-info)
                               (split-string " ## ")
                               (car)
                               (split-string ": ")
                               (last)
                               (car))))
      (when (not (string= (replace-regexp-in-string "/$" "" projectile-root) current-root))
        (let ((new-name (-> projectile-root
                            (split-string "/")
                            ((lambda (lst) (--remove (string= it "") lst)))
                            (last)
                            (car))))
          (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) new-name))))))

;; projectile
(use-package projectile
  :commands projectile
  ;; helm-projectile
  :bind (("C-c p h" . helm-projectile))
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " 📁 %s" (projectile-project-name)))))

;; eyebrowse
(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (add-hook 'find-file-hooks #'my-auto-set-projectile-root-to-eyebrowse))
