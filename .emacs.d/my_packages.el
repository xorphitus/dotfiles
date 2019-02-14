;;; my_packages.el --- pacakge installation

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; install packages

;;; Code:

(defmacro depends-on (name)
  `(use-package ,name :ensure t))

;; basic packages
(depends-on dash)
(depends-on drag-stuff)
(depends-on s)
(depends-on f)

(depends-on init-loader)
(depends-on use-package)

;; lisp
(depends-on parinfer)

;; clojure
(depends-on queue) ;; cider and clj-refactor requires
(depends-on cider)
(depends-on clj-refactor)
(depends-on clojure-mode)

;; common lisp
(depends-on slime)
(depends-on slime-company)

;; go
(depends-on go-mode)

;; haskell
(depends-on haskell-mode)

;; javascript / coffeescript / typescript
(depends-on coffee-mode)
(depends-on js2-mode)
(depends-on tide)
(depends-on vue-mode)

;; ruby
(depends-on rinari)
(depends-on ruby-electric)
(depends-on ruby-compilation)
;;(depends-on ruby-refactor)
(depends-on robe)
(depends-on haml-mode)
(depends-on scss-mode)
(depends-on slim-mode)
(depends-on sass-mode)

(depends-on yaml-mode)

;; rust
(depends-on rust-mode)

;; scala
(depends-on scala-mode)

;; c / c++
(depends-on google-c-style)

;; html
(depends-on emmet-mode)
(depends-on web-mode)
(depends-on less-css-mode)

;; markdown
(depends-on markdown-mode)

;; php
(depends-on php-mode)

;; shell
(depends-on fish-mode)

;; dockerfile
(depends-on dockerfile-mode)

;; git
(depends-on magit)
(depends-on gitignore-mode)
(depends-on gitconfig-mode)
(depends-on git-gutter-fringe)
(depends-on git-timemachine)

;; look and feel
(depends-on diminish)
(depends-on spacemacs-theme)
(depends-on spaceline)

;; visual effects
(depends-on highlight-indentation)
(depends-on rainbow-delimiters)
(depends-on volatile-highlights)

;; code jump
(depends-on projectile)
(depends-on helm-projectile)
(depends-on dumb-jump)
(depends-on helm-gtags)

;; other  utils
(depends-on ace-isearch)
(depends-on ace-window)
(depends-on anzu)
(depends-on company)
(depends-on ddskk)
(depends-on direnv)
(depends-on esup)
(depends-on expand-region)
(depends-on flycheck)
(depends-on helm)
(depends-on helm-ag)
(depends-on helm-ghq)
(depends-on helm-perspeen)
(depends-on helm-swoop)
(depends-on ido-vertical-mode)
(depends-on migemo)
(depends-on multiple-cursors)
(depends-on neotree)
(depends-on open-junk-file)
(depends-on perspeen)
(depends-on popup)
(depends-on protobuf-mode)
(depends-on quickrun)
(depends-on shell-pop)
(depends-on smartrep)
(depends-on undo-tree)
(depends-on visual-regexp-steroids)
(depends-on wgrep)
(depends-on wrap-region)
(depends-on yasnippet)
