;;; my_packages.el --- pacakge installation

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; install packages

;;; Code:

(defmacro depends-on (name)
  `(use-package ,name :ensure t))

;; javascript / typescript
(depends-on js2-mode)
(depends-on tide)
(depends-on vue-mode)

;; ruby
(depends-on ruby-electric)
(depends-on ruby-compilation)
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

;; kotlin
(depends-on kotlin-mode)

;; html
(depends-on emmet-mode)
(depends-on web-mode)
(depends-on less-css-mode)

;; php
(depends-on php-mode)

;; feed reader
(depends-on elfeed)
(depends-on elfeed-org)

;; look and feel
(depends-on all-the-icons)

;; other utils
(depends-on avy-migemo)
(depends-on esup)
