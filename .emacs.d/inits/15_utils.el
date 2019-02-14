;;; utils.el --- basic utility settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has basic utility settings.

;;; Code:

;; edit grep result directry
(use-package wgrep)

;; edit dired result directory
(use-package wdired
  :config
  (bind-key "C-c C-e" 'wdired-change-to-wdired-mode dired-mode-map))

;; comapny
(use-package company
  :diminish (company-mode . "🅒")
  :config
  (global-company-mode)
  (progn
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 2
          company-selection-wrap-around t)
    (setq company-dabbrev-downcase nil))

  (bind-keys :map company-mode-map
             ("C-i" . company-complete))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-search-words-regexp))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

;; undo-tree.el
(use-package undo-tree
  :diminish (undo-tree-mode . "🅤")
  :config
  (global-undo-tree-mode))

;; migemo
(use-package migemo
  :commands migemo
  :config
  (progn
    (setq migemo-command (if (executable-find "cmigemo") "cmigemo" "/usr/local/bin/cmigemo"))
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary
          (--find
           (f-exists? it)
           '("/usr/share/migemo/utf-8/migemo-dict"
             "/usr/share/cmigemo/utf-8/migemo-dict"
             "/usr/local/share/migemo/utf-8/migemo-dict")))
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (load-library "migemo")
    (migemo-init)))

;; flycheck
(use-package flycheck
  :diminish (flycheck-mode . "⚠")
  :config
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (if window-system
        (setq flycheck-display-errors-function
              '(lambda (errors)
                 (-when-let (messages (-keep #'flycheck-error-message errors))
                   (popup-tip (s-join "\n\n" messages))))))
    ;; modify flycheck indication of spaceline
    (setq spaceline-flycheck-bullet "⚠%s")))

;; junk file
(use-package open-junk-file
  :commands open-junk-file
  :config
  ;; open junk file in a current window
  (setq open-junk-file-find-file-function 'find-file))

;; magit.el
(use-package magit
  :commands magit
  :bind (("C-c g" . magit-status))
  :config
  (progn
    (add-hook 'magit-status-mode-hook (lambda ()
                                        (company-mode -1)))
    (setq magit-diff-refine-hunk t)))

;; quickrun.el
(use-package quickrun
  :commands quickrun
  :bind (([shift f5] . quickrun)))

;; shell-pop.el
(use-package shell-pop
  :commands shell-pop
  :bind (([f7] . shell-pop)))

;; neotree
(use-package neotree
  :bind (([f8] . neotree-toggle))
  :config
  (setq neo-smart-open t))

;;(setq projectile-switch-project-action 'neotree-projectile-action)
;;
;;(defun neotree-project-dir ()
;;  "Open NeoTree using the git root."
;;  (interactive)
;;  (let ((project-dir (projectile-project-root))
;;        (file-name (buffer-file-name)))
;;    (neotree-toggle)
;;    (if project-dir
;;        (if (neo-global--window-exists-p)
;;            (progn
;;              (neotree-dir project-dir)
;;              (neotree-find file-name)))
;;      (message "Could not find git project root."))))
;;
;;(global-set-key [f8] 'neotree-project-dir)


;; org-tree-slide
(use-package org-tree-slide
  :bind (([f9]       . org-tree-slide-mode)
         ([shift f9] . org-tree-slide-skip-done-toggle))
  :config
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil))

;; multiple-cursors and enhancers
(use-package multiple-cursors
  :bind (([C-M-return] . mc/edit-lines)))

(use-package expand-region
  :commands expand-region
  :bind (("C-,"   . er/expand-region)
         ("C-M-," . er/contract-region)))

(use-package smartrep
  :config
  (smartrep-define-key
      global-map "C-." '(("n" . 'mc/mark-next-like-this)
                         ("p" . 'mc/mark-previous-like-this)
                         ("P" . 'mc/unmark-next-like-this)
                         ("N" . 'mc/unmark-previous-like-this)
                         ("*" . 'mc/mark-all-like-this))))

;; anzu
(use-package anzu
  :diminish (anzu-mode . "🅐")
  :config
  (global-anzu-mode +1)
  (setq anzu-cons-mode-line-p nil))

;; ace-isearch
(use-package ace-isearch
  :diminish ace-isearch-mode
  :config (global-ace-isearch-mode 1))

;; ace-window
;; w/ http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (if (> (length (window-list)) 3)
      (ace-window 1)
      (other-window 1)))

(use-package ace-window
  :config
  (progn
    (global-set-key (kbd "C-x o") 'other-window-or-split)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

;; visual-regexp-steroids
(use-package visual-regexp-steroids)

;; electric-pair-mode
(electric-pair-mode t)

(use-package wrap-region
  :config
  (wrap-region-mode t))

(use-package direnv
 :config
 (direnv-mode))

;; ELDoc
(use-package eldoc
  :diminish (eldoc-mode . "📖"))
