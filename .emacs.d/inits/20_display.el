;;;; display.el --- display settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has display settings.

;;; Code:

;; basic theme settings
(use-package atom-one-dark-theme
  :init
  (load-theme 'atom-one-dark t))

;; skip startup screen
(setq inhibit-startup-screen t)

;; erase scrach buffer message
(setq initial-scratch-message "")

;; high light paren
(show-paren-mode 1)
;; high light inner text of paren when over window
(setq show-paren-style 'mixed)

;; high light current line
(setq hl-line-face 'underline)
(global-hl-line-mode)

;; tab-width
(setq default-tab-width 4)

;;; GUI settings

;; hide menu bar, tool bar and scroll bar
;; -> see .emacs.d/init.el

;; windmove
;; Shift + Arrow keys
;; http://d.hatena.ne.jp/tomoya/20120512/1336832436
(windmove-default-keybindings)

;; font
(set-frame-font "ricty-12")

;; set color, window size
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; line number
(global-linum-mode 1)
(setq linum-format "%4d.")

;; show spaces
(use-package whitespace
  :diminish (global-whitespace-mode . "🅦")
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
                        :background (face-attribute 'error :background)
                        :underline t)
    (set-face-attribute 'whitespace-tab nil
                        :foreground "LightSkyBlue"
                        :underline t)
    (set-face-attribute 'whitespace-space nil
                        :foreground "GreenYellow"
                        :weight 'bold)))

;; show an icon indicating whether a line has been changed
;; from last commit
(use-package git-gutter-fringe
  :diminish git-gutter-mode
  :config
  (progn
    (global-git-gutter-mode)
    (setq git-gutter-fr:side 'right-fringe)))

;; rainbow-delimiters
(use-package rainbow-delimiters)

;; highlight indentation
(use-package highlight-indentation
  :diminish highlight-indentation-mode)

;; mode line
(use-package doom-modeline
  :hook
  (after-init . doom-modeline-mode))

;; volatile-highlights
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; pretty-symbols
(global-prettify-symbols-mode +1)

;; diminish
;; * The modes writtem below are not refered by "use-package + :diminish"
;; * diminish.el is depended by use-package
(use-package diminish
  :config
  (diminish 'auto-revert-mode "⟳")
  (diminish 'view-mode "👁"))

