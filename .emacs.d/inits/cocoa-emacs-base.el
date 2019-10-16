;;; cocoa-emacs-base.el --- Mac OSX settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Mac OSX settings.

;;; Code:

(setq my-skk-jisyo-root "~/skk")

;; Need to place hunspell dictionaries
;; to the path which is included in the results of `hunspell -D`
;;
;; $ cd ~/Library/Spelling
;; $ wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff
;; $ wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic
(setq ispell-hunspell-dict-paths-alist '(("en_US" "~/Library/Spelling/en_US.aff"))
      ispell-dictionary "en_US")

;; hogee is me
;; this is mineee ok

;; beautify powerline
;; https://github.com/milkypostman/powerline/issues/54
(setq ns-use-srgb-colorspace nil)

(setq alert-default-style 'osx-notifier)
