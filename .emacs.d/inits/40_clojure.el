;;; Clojure
(unless (package-installed-p 'clojure-mode)
  (package-refresh-contents)
  (package-install 'clojure-mode))

;; nrepl
(unless (package-installed-p 'nrepl)
  (package-install 'nrepl))

;; paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
