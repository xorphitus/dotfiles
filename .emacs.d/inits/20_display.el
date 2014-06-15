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
(autoload 'whitespace "whitespace" nil t)
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
;; zenkaku space
(setq whitespace-space-regexp "\\(\x3000+\\)")
(global-whitespace-mode t)

(set-face-attribute 'whitespace-trailing nil
                    :foreground "DeepPink"
                    :background base-bgcolor
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :foreground "LightSkyBlue"
                    :background base-bgcolor
                    :underline t)
;; zenkaku space
(set-face-attribute 'whitespace-space nil
                    :foreground "GreenYellow"
                    :background base-bgcolor
                    :weight 'bold)

;; show an icon indicating whether a line has been changed
;; from last commit
(require 'git-gutter-fringe)
(global-git-gutter-mode)
(setq git-gutter-fr:side 'right-fringe)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)

;; golden-ratio
;; resizing automatically the windows you are working on
;; to the size specified in the "Golden Ratio"
(require 'golden-ratio)
(golden-ratio-mode 1)

;; rotate the window layout
(autoload 'rotate "rotate" nil t)
(global-set-key (kbd "C-t") 'rotate-layout)
(global-set-key (kbd "M-t") 'rotate-window)

;; highlight indentation
(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#131313")

;; powerline
(autoload 'powerline "powerline" nil t)
(powerline-default-theme)

;; smooth-scrolling
(autoload 'smooth-scrolling "smooth-scrolling" nil t)
