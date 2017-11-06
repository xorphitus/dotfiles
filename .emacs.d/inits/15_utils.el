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

;; flymake
(use-package flymake
  :config
  (progn
    ;; it's better to install flymakecursor.el for mouse less operation
    (load-library "flymake-cursor")
    ;; showmessage with popup
    ;; https://gist.github.com/292827
    (use-package popup
      :config
      (defun my-popup-flymake-display-error ()
        (interactive)
        (let* ((line-no            (flymake-current-line-no))
               (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
               (count              (length line-err-info-list)))
          (while (> count 0)
            (when line-err-info-list
              (let* ((file      (flymake-ler-file (nth (1- count) line-err-info-list)))
                     (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                     (text      (flymake-ler-text (nth (1- count) line-err-info-list)))
                     (line      (flymake-ler-line (nth (1- count) line-err-info-list))))
                (popup-tip (s-lex-format "[#{line}] #{text}"))))
            (setq count (1- count))))))))

;; flycheck
(use-package flycheck
  :config
  (progn
    (add-hook 'after-init-hook #'global-flycheck-mode)
    (if window-system
        (setq flycheck-display-errors-function
              '(lambda (errors)
                 (-when-let (messages (-keep #'flycheck-error-message errors))
                   (popup-tip (s-join "\n\n" messages))))))))

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
  :bind (([f8] . shell-pop)))

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
(global-anzu-mode +1)

;; ace-isearch
(global-ace-isearch-mode 1)

;; ace-window
;; w/ http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (ace-window 1))

(use-package ace-window
  :config
  (progn
    (global-set-key (kbd "C-x o") 'other-window-or-split)
    (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))))

;; visual-regexp-steroids
(use-package visual-regexp-steroids)

;; perspeen
(use-package perspeen
  :init
  (setq perspeen-use-tab t)
  :config
  (perspeen-mode))
