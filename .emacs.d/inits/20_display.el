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
     (:background "#000000"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
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
      (set-background-color "Black")
      (set-foreground-color "White")
      (set-cursor-color "LightGray")
      (set-frame-parameter nil 'alpha 80)
      ;; window size
      ;;(set-frame-parameter nil 'fullscreen 'fullboth)
      ))

;; line number
(global-linum-mode 1)
(set-face-attribute 'linum nil :foreground "#f00" :height 0.9)
(setq linum-format "%4d.")

;; clarify whitespace at line tails
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#ff0")

;; show an icon indicating whether a line has been changed
;; from last commit
(require 'git-gutter-fringe)
(global-git-gutter-mode)
(setq git-gutter-fr:side 'right-fringe)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
