;;; keys.el --- basic key binding settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has basic key binding settings.

;;; Code:

;;; disable dangerous keys
(bind-keys
 ("C-x C-c" . nil)
 ("C-x C-z" . nil))

;;; C-x C-c -> "exit" command
(defalias 'exit 'save-buffers-kill-emacs)

;;; Window splitting

;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(bind-key "C-z" 'other-window-or-split)

;;; bs-show
(bind-key "C-x C-b" 'bs-show)

;;; set key binds
(bind-key "C-h" 'delete-backward-char)
