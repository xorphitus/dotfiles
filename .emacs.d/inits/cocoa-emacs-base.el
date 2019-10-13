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

;; beautify powerline
;; https://github.com/milkypostman/powerline/issues/54
(setq ns-use-srgb-colorspace nil)

(setq alert-default-style 'osx-notifier)

