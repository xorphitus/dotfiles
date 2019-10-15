;;; completion.el --- helm settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has completion settings.

;;; Code:
(use-package helm
  :diminish helm-mode)

(use-package helm-config
  :after (helm)
  :bind (("C-c h" . helm-for-files))
  :init
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
  (bind-key "C-h" 'delete-backward-char helm-map))

;; enable helm + migemo
(use-package migemo
  :after (helm helm-config)
  :diminish (helm-migemo-mode . "🅗🅜")
  :config (helm-migemo-mode 1))

;; helm-ag
(use-package helm-ag
  :bind
  (("M-g ." . helm-ag)
   ("M-g ," . helm-ag-pop-stack))
  :custom
  (helm-ag-base-command "rg --no-heading"))

(use-package helm-rg
  :bind
  (("M-G ." . helm-rg)))

;; ivy/counsel settings
(use-package ivy
  :diminish (ivy-mode . "🅘")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :after (ivy)
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-b" . counsel-ibuffer))
  :custom
  (counsel-yank-pop-separator "\n――――――――\n"))

(use-package swiper
  :after (ivy))

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

(use-package ivy-posframe
  :after (ivy)
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
  (ivy-posframe-mode 1))

(use-package ivy-prescient
  :after (ivy)
  :config
  (ivy-prescient-mode))

(defun counsel-ghq (&optional initial-input)
  "Open a file using the ghq shell command."
  (interactive)
  (let ((candidates (split-string
                     (shell-command-to-string
                      "ghq list --full-path")
                     "\n")))
    (ivy-read "ghq: "
              candidates
              :initial-input initial-input
              :action #'find-file
              :caller 'counsel-ghq)))
