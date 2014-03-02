;; change quit and supend operation
;; C-x C-c -> "exit" command
(global-set-key (kbd "C-x C-c") nil)
(global-set-key (kbd "C-x C-z") nil)
(defalias 'exit 'save-buffers-kill-emacs)

;; Window splitting

;; http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (split-window-horizontally))
  (other-window 1))

(global-set-key (kbd "C-z") 'other-window-or-split)

;; ibuffer
(global-set-key (kbd "C-x C-b")  'ibuffer)

;; set key binds
(global-set-key "\C-h" 'delete-backward-char)

;; ddskk
(global-set-key (kbd "C-x C-j") 'skk-mode)
