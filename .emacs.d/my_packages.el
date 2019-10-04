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

;; lisp
(depends-on paredit)
(depends-on parinfer)

;; clojure
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
(depends-on rustic)

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

;; org-mode
(depends-on org-alert)
(depends-on org-bullets)
(depends-on org-pomodoro)
(depends-on sound-wav) ;; required by org-pomodoro implicitly

;; look and feel
(depends-on all-the-icons)
(depends-on atom-one-dark-theme)
(depends-on dashboard)
(depends-on diminish)
(depends-on doom-modeline)

;; visual effects
(depends-on hide-mode-line)
(depends-on highlight-indentation)
(depends-on rainbow-delimiters)
(depends-on volatile-highlights)

;; code jump
(depends-on dumb-jump)
(depends-on helm-gtags)
(depends-on smart-jump)

;; files and projects
(depends-on eyebrowse)
(depends-on helm-projectile)
(depends-on projectile)
(depends-on treemacs)
(depends-on treemacs-icons-dired)
(depends-on treemacs-magit)
(depends-on treemacs-projectile)

;; other utils
(depends-on ace-isearch)
(depends-on ace-window)
(depends-on alert)
(depends-on anzu)
(depends-on company)
;; (depends-on ddskk) ;; it causes an error at emacs initialization
(depends-on direnv)
(depends-on eglot)
(depends-on esup)
(depends-on expand-region)
(depends-on flycheck)
(depends-on flycheck-popup-tip)
(depends-on helm)
(depends-on helm-ag)
(depends-on helm-descbinds)
(depends-on helm-ghq)
(depends-on helm-rg)
(depends-on helm-swoop)
(depends-on ido-vertical-mode)
(depends-on migemo)
(depends-on multiple-cursors)
(depends-on open-junk-file)
(depends-on popup)
(depends-on protobuf-mode)
(depends-on quickrun)
(depends-on shackle)
(depends-on shell-pop)
(depends-on smartrep)
(depends-on undo-tree)
(depends-on visual-regexp-steroids)
(depends-on wgrep)
(depends-on which-key)
(depends-on wrap-region)
(depends-on yasnippet)
