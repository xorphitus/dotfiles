;;; xorphitus elisp

;;; general

;; enable common lisp
(require 'cl)

(setq explicit-shell-file-name shell-file-name)

;; set load path
;;(add-to-list 'load-path "~/.emacs.d/elisp")
(let ((default-directory "~/.emacs.d/elisp/"))
   (setq load-path (cons default-directory load-path))
    (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/elpa/"))
    (setq load-path (cons default-directory load-path))
    (normal-top-level-add-subdirs-to-load-path))

;; reload ~/.emacs/init.d
(defun reload-elisp ()
  (interactive)
  (load-library "~/.emacs/init.el"))

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;; look and feel

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

;; styles for coding
(dolist (hook (list
               'text-mode-hook
	       'c-mode-common-hook
	       'emacs-lisp-mode-hook
	       'lisp-interaction-mode-hook
	       'lisp-mode-hook
	       'python-mode-hook
	       'coffee-mode-hook
	       'js2-mode-hook
               'scala-mode-hook
               'ruby-mode-hook
               'haml-mode-hook
	       ))
  (add-hook hook (lambda ()
                   ;; line number
		   (linum-mode t)
		   (set-face-attribute 'linum nil :foreground "#f00" :height 0.9)
		   (setq linum-format "%4d.")
                   ;; clarify whitespace at line tails
                   (setq show-trailing-whitespace t)
                   (set-face-background 'trailing-whitespace "#ff0")
		   )))

;; indent
(setq-default indent-tabs-mode nil)

;; quickrun.el
;; http://d.hatena.ne.jp/syohex/20111201/1322665378
(require 'quickrun)
(global-set-key [(shift f5)] 'quickrun)

;; shell-pop.el
;;  http://www.emacswiki.org/emacs/download/shell-pop.el
(require 'shell-pop)
(global-set-key [f8] 'shell-pop)

;; magit.el
(require 'magit)

;; tabber.el
;;(if window-system
;;    (progn
;;      (require 'tabbar)
;;      (global-set-key [C-tab] 'tabbar-forward)
;;      (global-set-key [(control shift iso-lefttab)] 'tabbar-backward)
;;      (tabbar-mode)
;;      )
;;  )

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

;; Japanese input

;; mozc
;;(require 'mozc)
;;(set-language-environment "Japanese")
;;(setq default-input-method "japanese-mozc")
;;(global-set-key (kbd "C-o") 'toggle-input-method)

;; anthy
(load-library "anthy")
(setq default-input-method "japanese-anthy")

;; e2wm
;;  http://d.hatena.ne.jp/kiwanami/20100528/1275038929
;;  require: window-layout.el, e2wm.el
(require 'e2wm)
(global-set-key (kbd "M-+") 'e2wm:start-management)
;(if window-system
;    (e2wm:start-management)
;)

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
      )
)

;; change look and feel with powerline.el
;;  http://www.emacswiki.org/emacs/powerline.el
;;
;; can't see 'arrow style' mode-line w/ -nw option.
(defun arrow-right-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\".	c %s\",
\" 	c %s\",
\".           \",
\"..          \",
\"...         \",
\"....        \",
\".....       \",
\"......      \",
\".......     \",
\"........    \",
\".........   \",
\".........   \",
\"........    \",
\".......     \",
\"......      \",
\".....       \",
\"....        \",
\"...         \",
\"..          \",
\".           \"};"  color1 color2))

(defun arrow-left-xpm (color1 color2)
  "Return an XPM right arrow string representing."
  (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\".	c %s\",
\" 	c %s\",
\"           .\",
\"          ..\",
\"         ...\",
\"        ....\",
\"       .....\",
\"      ......\",
\"     .......\",
\"    ........\",
\"   .........\",
\"   .........\",
\"    ........\",
\"     .......\",
\"      ......\",
\"       .....\",
\"        ....\",
\"         ...\",
\"          ..\",
\"           .\"};"  color2 color1))


(defconst color1 "#00c")
(defconst color2 "#009")

(defvar arrow-right-1 (create-image (arrow-right-xpm color1 color2) 'xpm t :ascent 'center))
(defvar arrow-right-2 (create-image (arrow-right-xpm color2 "None") 'xpm t :ascent 'center))
(defvar arrow-left-1  (create-image (arrow-left-xpm color2 color1) 'xpm t :ascent 'center))
(defvar arrow-left-2  (create-image (arrow-left-xpm "None" color2) 'xpm t :ascent 'center))

(setq-default mode-line-format
 (list  '(:eval (concat (propertize " %b " 'face 'mode-line-color-1)
                        (propertize " " 'display arrow-right-1)))
        '(:eval (concat (propertize " %m " 'face 'mode-line-color-2)
                        (propertize " " 'display arrow-right-2)))

        ;; Justify right by filling with spaces to right fringe - 16
        ;; (16 should be computed rahter than hardcoded)
        '(:eval (propertize " " 'display '((space :align-to (- right-fringe 17)))))

        '(:eval (concat (propertize " " 'display arrow-left-2)
                        (propertize " %p " 'face 'mode-line-color-2)))
        '(:eval (concat (propertize " " 'display arrow-left-1)
                        (propertize "%4l:%2c  " 'face 'mode-line-color-1)))
))

(make-face 'mode-line-color-1)
(set-face-attribute 'mode-line-color-1 nil
                    :foreground "#fff"
                    :background color1)

(make-face 'mode-line-color-2)
(set-face-attribute 'mode-line-color-2 nil
                    :foreground "#fff"
                    :background color2)

(set-face-attribute 'mode-line nil
                    :foreground "#fff"
                    :background "#000"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "#fff"
                    :background "#000")

;;; other behaviors

;; change C-a behavior
;; move line head w/o indent, w/ indent
;; http://d.hatena.ne.jp/gifnksm/20100131/1264956220
(defun beginning-of-indented-line (current-point)
  (interactive "d")
  (if (string-match
       "^[ ¥t]+$"
       (save-excursion
	 (buffer-substring-no-properties
	  (progn (beginning-of-line) (point))
	  current-point)))
      (beginning-of-line)
    (back-to-indentation)))
(global-set-key "\C-a" 'beginning-of-indented-line)

;; without backup-file
(setq backup-inhibited t)

;; delete auto save files when quit
(setq delete-auto-save-files t)

;; set key binds
(global-set-key "\C-h" 'delete-backward-char)

;; mermelada - elisp repository
;;  http://marmalade-repo.org/
;;  'package-list-packages
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;; rectanble select
;;  GUI: C-Ret
;;  CUI: 'cua-set-rectangle-mark
;; M-n -> insert numbers incremental
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; edit grep result directry
;;  https://raw.github.com/mhayashi1120/Emacs-wgrep/master/wgrep.el
(require 'wgrep)

;; edit dired result directory
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; auto-install.el
;;  http://www.emacswiki.org/emacs/download/auto-install.el
(when (require 'auto-install nil t)
    (setq auto-install-directory "~/.emacs.d/elisp/")
    (auto-install-update-emacswiki-package-name t)
    (auto-install-compatibility-setup))

;; anything.el
;;  http://www.emacswiki.org/cgi-bin/wiki/download/anything.el
;;  http://www.emacswiki.org/cgi-bin/wiki/download/anything-config.el
(require 'anything-startup)
(require 'color-theme)

;; auto-complete
;;  http://cx4a.org/software/auto-complete/index.ja.html
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet-0.6.1c")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/plugins/yasnippet-0.6.1c/snippets")

;; redo+
;;  http://www.emacswiki.org/emacs/download/redo+.el
(require 'redo+)

;; migemo
;(require 'migemo)

;; evernote
;(require 'evernote-mode)

;; flymake
(require 'flymake)
;; it's better to install flymakecursor.el for mouse less operation
(when (load "flymake" t)
    (load-library "flymake-cursor"))
;; showmessage with popup
;; https://gist.github.com/292827
(require 'popup)
(defun my-popup-flymake-display-error ()
    (interactive)
    (let* ((line-no            (flymake-current-line-no))
           (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (count              (length line-err-info-list)))
        (while (> count 0)
            (when line-err-info-list
                (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
                (full-file (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                (text      (flymake-ler-text (nth (1- count) line-err-info-list)))
                (line      (flymake-ler-line (nth (1- count) line-err-info-list))))
            (popup-tip (format "[%s] %s" line text))))
            (setq count (1- count)))))

;;; Database

;; edbi
;;  http://d.hatena.ne.jp/kiwanami/20120305/1330939440
(require 'edbi)
(autoload 'edbi:open-db-viewer "edbi")

;;;
;;; JavaScript

;; js2-mode (forked by id:mooz, fixing identation)
;;  https://raw.github.com/mooz/js2-mode/master/js2-mode.el
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;
;;; CoffeeScript

;; cofee-mode
;; https://github.com/defunkt/coffee-mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(add-hook 'coffee-mode-hook
    '(lambda()
        (set (make-local-variable 'tab-width) 2)))

;;;
;;; ActionScript

;; actionscript-mode
;; (actionscript-mode.el)
(autoload 'actionscript-mode "actionscript-mode" "actionscript" t)
(setq auto-mode-alist
	(append '(("\\.as$" . actionscript-mode)) auto-mode-alist))

;;;
;;; Python

;; python-mode
;; (python-mode.el)
(require 'python-mode)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook
      '(lambda()
            (setq indent-tabs-mode nil)
            (setq indent-level 4)
            (setq python-indent 4)
            (setq tab-width 4)))


;; ipython and completion
;;  required package: ipython, ipython.el, anything-ipyton.el
(add-hook 'python-mode-hook
    (lambda ()
        (setq py-python-command "/usr/bin/ipython")
        (require 'ipython)
        (require 'anything-ipython)
        (when (require 'anything-show-completion nil t)
            (use-anything-show-completion 'anything-ipython-complete
               '(length initial-pattern)))))

;(add-hook 'python-mode-hook
;    ;; for outline-mode
;    (lambda ()
;        (set (make-variable-buffer-local 'beginning-of-defun-function)
;            'py-beginning-of-def-or-class)
;        (setq outline-regexp "def\\|class "))
;    ;; for ElDoc
;    '(lambda () (eldoc-mode 1)) t
;)

;(defun py-next-block ()
;    "go to the next block.  Cf. `forward-sexp' for lisp-mode"
;    (interactive)
;    (py-mark-block nil 't)
;    (back-to-indentation))

;; flymake
;;  required package: pylint, pyflakes, python-mode.el
(when (load "flymake" t)
    ;; use pylint
    ;; require installing pylint (epylint)
    (defun flymake-pylint-init ()
        (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
            (local-file (file-relative-name temp-file (file-name-directory buffer-file-name ))))
            (list "epylint" (list local-file))))
    (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pylint-init))
)
;(when (load "flymake" t)
;    ;; use pyflake
;    ;; require installint pyflakes
;    (defun flymake-pyflakes-init ()
;        ; make sure it's not a remote buffer or flymake would not work
;        (when (not (subsetp (list (current-buffer)) (tramp-list-remote-buffers)))
;            (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
;                (local-file (file-relative-name temp-file (file-name-directory buffer-file-name ))))
;                (list "pyflakes" (list local-file)))))
;    (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init))
;)

(add-hook 'python-mode-hook
	  '(lambda ()
	     (flymake-mode t)))

;; rope
;;  required package: pymacs, ropemode
;(add-hook 'python-mode-hook
;    (lambda ()
;        (require 'pymacs)
;        (pymacs-load "ropemacs" "rope-")
;        (setq ropemacs-enable-autoimport t)))


(defun pybrew ()
  (interactive)
  (message (concat "echo " (read-from-minibuffer))))
  ;(shell-command (concat "echo " (read-from-minibuffer))))
  ;(shell-command (concat "pythonbrew " (buffer-time-stamp))))

;;;
;;; C/C++

;; indent
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (c-toggle-auto-hungry-state 1)
	     (define-key c-mode-base-map "\C-m" 'newline-and-indent)))

;; auto spell check by ispell
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (flyspell-prog-mode)))
(setq flyspell-issue-welcome-flag nil)

;; flymake without Makefile
(defun flymake-cc-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "g++" (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (flymake-mode t)))

;; compile C-c
(add-hook 'c-mode-common-hook
	  '(lambda ()
	     (define-key mode-specific-map "c" 'compile)))

;;;
;;; Erlang

;; define env ERLANG_HOME
(cond ((file-exists-p (concat (getenv "ERLANG_HOME") "/lib/tools-2.6.6.1/emacs"))
    (setq load-path (cons  (concat (getenv "ERLANG_HOME") "/lib/tools-2.6.6.1/emacs") load-path))
    (setq erlang-root-dir (getenv "ERLANG_HOME"))
    (setq exec-path (cons (concat (getenv "ERLANG_HOME") "/bin") exec-path))
    (require 'erlang-start)
))


;;; Haskell

;; require: haskell-platform
;(load "haskell-site-file")
;(add-fook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-fook 'haskell-mode-hook 'turn-on-haskell-identation-mode)


;;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.text" . markdown-mode) auto-mode-alist))
(put 'dired-find-alternate-file 'disabled nil)


;;; Scala

;; scala-mode
(require 'scala-mode-auto)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

;; ;; requre ensime
;; ;;  https://github.com/aemoncannon/ensime
;; (add-to-list 'load-path "/usr/share/ensime/elisp")
;; (add-to-list 'exec-path "/usr/share/ensime")
;; (require 'ensime)
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)


;;; Ruby

;; ruby-block.el
(require 'ruby-block)
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake" . ruby-mode))
(ruby-block-mode t)
(setq ruby-block-highlight-toggle t)

;; flymake-ruby
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;; Rinari
;;  https://github.com/eschulte/rinari
(require 'rinari)

;;; CSS
(add-to-list 'auto-mode-alist '("\\.less" . css-mode))

;;; haml
(require 'haml-mode)
(require 'flymake-haml)
(add-hook 'haml-mode-hook 'flymake-haml-load)
