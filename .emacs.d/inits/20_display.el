;;; display.el --- display settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has display settings.

;;; Code:

;; common variables
(defvar base-bgcolor "#000000")

;; skip startup screen
(setq inhibit-startup-screen t)

;; erase scrach buffer message
(setq initial-scratch-message "")

;; hide menu bar
(menu-bar-mode -1)

;; high light paren
(show-paren-mode 1)
;; high light inner text of paren when over window
(setq show-paren-style 'mixed)

;; high light current line
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "Black"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'underline)
(global-hl-line-mode)

;;; GUI settings

;; windmove
;; Shift + Arrow keys
;; http://d.hatena.ne.jp/tomoya/20120512/1336832436
(windmove-default-keybindings)

;; hide scroll bar
(set-scroll-bar-mode nil)

;; hide menu bar
(menu-bar-mode -1)

;; hide toolbar
(tool-bar-mode -1)

;; font
(set-frame-font "ricty-12")

;; set color, window size
(if window-system
    (progn
      ;; color
      (set-background-color base-bgcolor)
      (set-foreground-color "White")
      (set-cursor-color "LightGray")
      (set-frame-parameter nil 'alpha 90)
      ;; window size
      ;;(set-frame-parameter nil 'fullscreen 'fullboth)
      ))

;; line number
(global-linum-mode 1)
(set-face-attribute 'linum nil :foreground "#f00" :height 0.9)
(setq linum-format "%4d.")

;; show spaces
(use-package whitespace
  :commands whitespace
  :init
  (progn
    (setq whitespace-style
          '(face
            tabs
            tab-mark
            spaces
            lines-tail
            trailing
            space-before-tab
            space-after-tab::space))
    (setq whitespace-line-column 250)
    (setq whitespace-space-regexp "\\(\x3000+\\)") ; zenkaku space
    (global-whitespace-mode t)
    (set-face-attribute 'whitespace-trailing nil
                        :foreground "DeepPink"
                        :background base-bgcolor
                        :underline t)
    (set-face-attribute 'whitespace-tab nil
                        :foreground "LightSkyBlue"
                        :background base-bgcolor
                        :underline t)
    (set-face-attribute 'whitespace-space nil
                        :foreground "GreenYellow"
                        :background base-bgcolor
                        :weight 'bold)))

;; show an icon indicating whether a line has been changed
;; from last commit
(use-package git-gutter-fringe
  :config
  (progn
    (global-git-gutter-mode)
    (setq git-gutter-fr:side 'right-fringe)))

;; rainbow-delimiters
(use-package rainbow-delimiters
  :config
  ;; color settings
  (use-package color
    :config
    (-each (number-sequence 1 rainbow-delimiters-max-face-count)
      (lambda (index)
        (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
          (callf color-saturate-name (face-foreground face) 90))))))

;; golden-ratio
;; resizing automatically the windows you are working on
;; to the size specified in the "Golden Ratio"
(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

;; rotate the window layout
(use-package rotate
  :commands rotate
  :bind (("C-t" . rotate-layout)
         ("M-t" . rotate-window)))

;; highlight indentation
;(require 'highlight-indentation)
;(set-face-background 'highlight-indentation-face "#131313")
;(set-face-background 'highlight-indentation-current-column-face "#1f1f1f")
;(add-hook 'highlight-indentation-mode-hook 'highlight-indentation-current-column-mode)

;; powerline
(use-package powerline
  :config
  (powerline-default-theme))

;; smooth-scrolling
(use-package smooth-scrolling
  :commands smooth-scrolling)

;; volatile-highlights
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; pretty-symbols
(global-prettify-symbols-mode +1)
