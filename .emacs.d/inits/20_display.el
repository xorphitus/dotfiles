;;;; display.el --- display settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has display settings.

;;; Code:

;; basic theme settings
(use-package spacemacs-theme
  :init
  (custom-set-variables '(spacemacs-theme-custom-colors
                          '((base . "#dddddd"))))
  (load-theme 'spacemacs-dark t))

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
(use-package rainbow-delimiters
  :config
  ;; color settings
  (use-package color
    :config
    (--each (number-sequence 1 rainbow-delimiters-max-face-count)
      (let ((face (intern (format "rainbow-delimiters-depth-%d-face" it))))
        (callf color-saturate-name (face-foreground face) 90)))))

;; rotate the window layout
(use-package rotate
  :commands rotate
  :bind (("C-S-t l" . rotate-layout)
         ("C-S-t w" . rotate-window)))

;; highlight indentation
(use-package highlight-indentation
  :diminish highlight-indentation-mode)

;; powerline
(use-package spaceline-config
  :config
  (spaceline-spacemacs-theme))

;; volatile-highlights
(use-package volatile-highlights
  :config
  (volatile-highlights-mode t))

;; pretty-symbols
(global-prettify-symbols-mode +1)
