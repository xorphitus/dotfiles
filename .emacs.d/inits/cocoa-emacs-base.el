;;; cocoa-emacs-base.el --- Mac OSX settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Mac OSX settings.

;;; Code:

;; (set-face-attribute 'default nil :family "Ricty" :height 130)
;; (set-fontset-font "fontset-default" 'japanese-jisx0208 '("Ricty"))

(setq default-frame-alist
      '((font . "ricty-14")))
(set-frame-font "ricty-14")

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

