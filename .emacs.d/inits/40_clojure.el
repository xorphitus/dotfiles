;;; Clojure
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))

;;; paredit
(add-hook 'clojure-mode-hook 'paredit-mode)


;;; CIDER
(unless (package-installed-p 'cider)
  (package-install 'cider))

;; ;; Enable eldoc in Clojure buffers
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; ;; Hide the *nrepl-connection* and *nrepl-server* buffers
;; ;; from appearing in some buffer switching commands like 'C-x b'
;; (setq nrepl-hide-special-buffers t)

;; ;; Control the TAB key behavior in the REPL
;; (setq cider-repl-tab-command 'indent-for-tab-command)

;; ;; Prevent the auto-display of the REPL buffer in a separate window after connection is established
;; (setq cider-repl-pop-to-buffer-on-connect nil)

;; ;; Stop the error buffer from popping up while working in buffers other than the REPL
;; (setq cider-popup-stacktraces nil)

;; ;; Enable error buffer popping also in the REPL
;; (setq cider-repl-popup-stacktraces t)

;; ;; To auto-select the error buffer when it's displayed
;; (setq cider-auto-select-error-buffer t)

;; ;; The REPL buffer name  will look like cider project-name:port
;; (setq nrepl-buffer-name-show-port t)

;; ;; Make 'C-c C-z' switch to the CIDER REPL buffer in the current window
;; (setq cider-repl-display-in-current-window t)

;; ;; To make the REPL history wrap around when its end is reached
;; (setq cider-repl-wrap-history t)

;; paredit-mode
(add-hook 'cider-repl-mode-hook 'paredit-mode)
