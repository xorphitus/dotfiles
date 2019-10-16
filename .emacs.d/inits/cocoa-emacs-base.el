;;; cocoa-emacs-base.el --- Mac OSX settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has Mac OSX settings.

;;; Code:

;; Need to place hunspell dictionaries
;; to the path which is included in the results of `hunspell -D`
;;
;; $ cd ~/Library/Spelling
;; $ wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff
;; $ wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic
(setq  ispell-dictionary "en_US")
(add-to-list 'ispell-dictionary-alist
             '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en_US") nil utf-8))
;; https://stackoverflow.com/questions/3961119/working-setup-for-hunspell-in-emacs
(defun ispell-get-coding-system () 'utf-8)

;; beautify powerline
;; https://github.com/milkypostman/powerline/issues/54
(setq ns-use-srgb-colorspace nil)

(setq alert-default-style 'osx-notifier)
