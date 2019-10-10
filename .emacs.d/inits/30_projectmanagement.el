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
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " 📁 %s" (projectile-project-name)))))

;; counsel-projectile
(use-package counsel-projectile
  :config
  (counsel-projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; eyebrowse
(use-package eyebrowse
  :config
  (eyebrowse-mode t)
  (add-hook 'find-file-hooks #'my-auto-set-projectile-root-to-eyebrowse))

;; awesome-tab
(use-package awesome-tab
  :bind
  (("C-c <M-right>" . awesome-tab-forward)
   ("C-c <M-left>" .  awesome-tab-backward)
   ("C-c <M-down>" .  awesome-tab-forward-group)
   ("C-c <M-up>" .    awesome-tab-backward-group)
   ("C-c M-z" . awesome-tab-switch-group)
   ("C-c M-1" . awesome-tab-select-visible-tab)
   ("C-c M-2" . awesome-tab-select-visible-tab)
   ("C-c M-3" . awesome-tab-select-visible-tab)
   ("C-c M-4" . awesome-tab-select-visible-tab)
   ("C-c M-5" . awesome-tab-select-visible-tab)
   ("C-c M-6" . awesome-tab-select-visible-tab)
   ("C-c M-7" . awesome-tab-select-visible-tab)
   ("C-c M-8" . awesome-tab-select-visible-tab)
   ("C-c M-9" . awesome-tab-select-visible-tab))
  :config
  (awesome-tab-mode t))
