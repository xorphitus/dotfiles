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

(use-package company-box
  :hook
  (company-mode . company-box-mode))

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
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq spaceline-flycheck-bullet "⚠%s"))

(use-package flycheck-popup-tip
  :after (flycheck)
  :config
  (flycheck-popup-tip-mode)
  :custom-face
  (popup-tip-face ((t (:foreground "#f1fa8c"
                       :background "#111111")))))

;; junk file
(use-package open-junk-file
  :commands open-junk-file
  :config
  ;; open junk file in a current window
  (setq open-junk-file-find-file-function 'find-file))

;; magit.el
(use-package magit
  :commands magit
  ;; same as IntelliJ IDEA short cut
  :bind (("M-9" . magit-status))
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
  ;; same as IntelliJ IDEA short cut
  :bind (([M-f12] . shell-pop)))

;; treemacs
(use-package treemacs
  :defer t
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ;; same as IntelliJ IDEA short cut
        ("M-1" . treemacs)))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

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
  :config
  (global-ace-isearch-mode 1)
  :custom
  (ace-isearch-function 'avy-goto-char)
  ;;(ace-isearch-function-from-isearch 'ace-isearch-swiper-from-isearch)
  (ace-isearch-function-from-isearch 'helm-swoop-from-isearch)
  )

;; ace-window
;; w/ http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (if (one-window-p)
      (split-window-horizontally)
    (ace-window 1)))

(use-package ace-window
  :config
  (global-set-key (kbd "C-x o") 'other-window-or-split)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c")))))

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

;; shackle
;; popup interface
(use-package shackle
  :config
  (shackle-mode 1)
  (setq shackle-default-rule '(:other t))
  (setq helm-display-function #'pop-to-buffer) ; make helm play nice
  (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t     :size 0.4)
                        ("*undo-tree*"         :regexp t :align right :size 0.25)
                        ("*Backtrace*"         :regexp t :align t     :size 0.4)
                        ("*Warnings*"          :regexp t :align t     :size 0.4)
                        ("*cider-error*"    :inhibit-window-quit t)
                        ("\\`\\*magit.*?\\*\\'" :regexp t :select t   :inhibit-window-quit t :same t)
                        )))

;; which-key
(use-package which-key
  :config
  (which-key-mode))
