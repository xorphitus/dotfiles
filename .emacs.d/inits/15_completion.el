;;; completion.el --- completion settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has completion settings.

;;; Code:

;; enable completion + migemo
;; TODO: It causes problems for avy and swiper: the following pull request fixes it?
;; https://github.com/momomo5717/avy-migemo/pull/8
;;(use-package avy-migemo
;;  :config
;;  (avy-migemo-mode 1)
;;  (require 'avy-migemo-e.g.swiper))

;; ivy/counsel settings
(use-package ivy
  :diminish (ivy-mode . "🅘")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(defun my-counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "This is a counsel-rg alternative. It searches a text in the current directory.
It you need to search a text in a project root directory,
use projectile-counsel-rg instead."
  (interactive)
  (let ((counsel-ag-base-command
         (if (listp counsel-rg-base-command)
             (append counsel-rg-base-command (counsel--rg-targets))
           (concat counsel-rg-base-command " "
                   (mapconcat #'shell-quote-argument (counsel--rg-targets) " "))))
        (counsel--grep-tool-look-around
         (let ((rg (car (if (listp counsel-rg-base-command) counsel-rg-base-command
                          (split-string counsel-rg-base-command))))
               (switch "--pcre2"))
           (and (eq 0 (call-process rg nil nil nil switch "--pcre2-version"))
                switch))))
    (counsel-ag initial-input default-directory extra-rg-args rg-prompt
                :caller 'counsel-rg)))

;; How to edit the result lines
;;
;; 1. search with swiper, counsel-rg, etc.
;; 2. (optional) ivy-avy (C-') : search a target
;; 3. ivy-occur (C-c C-o) : start occur to edit
;; 4. ivy-wgrep-change-to-wgrep-mode (C-x C-q) : start editing
;; 5. wgrep-finish-edit (C-c C-c) : commit
;;
;; How to change counsel-rg directory
;;
;; 1. search with cousel-rg which searches in a project root directory
;; 2. counsel-cd (C-x C-d)
(use-package counsel
  :after (ivy)
  :bind (("C-c h" . counsel-recentf)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-S-f" . my-counsel-rg)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-b" . counsel-ibuffer))
  :config
  ;; The following glob setting doesn't work. Maybe it should be invoked later
  ;; (push '(counsel-rg . "--glob '**' -- ") ivy-initial-inputs-alist)
  (ivy-set-actions
   'my-counsel-rg
   '(("j" counsel-find-library-other-window "other window")
     ("f" counsel-find-library-other-frame "other frame")))
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
  (setq ivy-posframe-display-functions-alist
        '((swiper . nil)
          (t      . ivy-posframe-display-at-window-center)))
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
