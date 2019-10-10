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

;;; set key binds
(bind-key "C-h" 'delete-backward-char)
