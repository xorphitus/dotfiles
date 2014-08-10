;; change quit and supend operation

;; disable keys
(--each '("C-x C-c"
          "C-x C-z")
  (global-set-key (kbd it) nil))
;; C-x C-c -> "exit" command
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
(--each '("C-o"
          "C-x C-j")
  (global-set-key (kbd it) 'skk-mode))
