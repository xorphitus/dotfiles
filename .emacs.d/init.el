;;; init.el --- xorphitus elisp

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This is an entry point of xorphitus elisp.

;;; Code:

;; GC max memory (128MB)
(setq gc-cons-threshold 134217728)


;; hide basic gui widgets first
;; I don't want to show them in initialing
(menu-bar-mode -1)
(when window-system
  (tool-bar-mode -1)
  (set-scroll-bar-mode nil))

;; set load path
(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; leaf
(defun install-leaf ()
  (eval-and-compile
    (customize-set-variable
     'package-archives '(("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
    (package-initialize)
    (unless (package-installed-p 'leaf)
      (package-refresh-contents)
      (package-install 'leaf))

    (leaf leaf-keywords
      :ensure t
      :init
      ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
      ;; (leaf hydra :ensure t)
      ;; (leaf el-get :ensure t)
      ;; (leaf blackout :ensure t)

      :config
      ;; initialize leaf-keywords.el
      (leaf-keywords-init))))

(install-leaf)
(leaf leaf-tree :ensure t)
(leaf leaf-convert :ensure t)

;; Basic packages
(leaf dash
  :ensure t
  :require t)
(leaf drag-stuff
  :ensure t
  :require t)
(leaf s
  :ensure t
  :require t)
(leaf f
  :ensure t
  :require t)

;; Constants
(defconst my-const/lambda-prettify-symbols-alist
  '(("lambda" . ?λ)))

(defconst my-const/relational-prettify-symbols-alist
  '(("!=" . ?≠)
    ("/=" . ?≠)
    (">=" . ?≥)
    ("<=" . ?≤)))

(defconst my-const/logical-prettify-symbols-alist
  '(("and" . ?∧)
    ("or"  . ?∨)))

(defconst my-const/logical-prettify-symbols-ext-alist
  (-concat my-const/lambda-prettify-symbols-alist
           '(("not" . ?¬)
             ("nil" . ?∅))))

(defconst my-const/arror-prettify-symbols-alist
  '(("->" . ?→)
    ("=>" . ?⇒)))

;; Macros
(defmacro my-macro/prettify-symbols (hook symbols-alist)
  `(add-hook ',hook
             (lambda ()
               (-each ,symbols-alist
                 (lambda (prettify-map)
                   (push prettify-map prettify-symbols-alist))))))

;; show explicit file name
(setq explicit-shell-file-name shell-file-name)

;; yes/no -> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; file name completion ignore case
(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; auto reload buffer which modified by external programs
(global-auto-revert-mode 1)

;; enable ido
;; to improbe C-x C-f
(ido-mode 1)
(setq confirm-nonexistent-file-or-buffer nil)

;; indent
(setq-default indent-tabs-mode nil)

;; change C-a behavior
;; move line head w/o indent, w/ indent
;; http://d.hatena.ne.jp/gifnksm/20100131/1264956220
(defun beginning-of-indented-line (current-point)
  (interactive "d")
  (if (s-match "^[ \t]+$"
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

;; rectanble select
;;  GUI: C-Ret
;;  CUI: 'cua-set-rectangle-mark
;; M-n -> insert numbers incremental
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; disable beep sound flash
(setq ring-bell-function 'ignore)

;; For time locale for org-mode to avoid Japanese time locale format
;; However this is not an org-mode specific setting but a global setting, written here
(setq system-time-locale "C")

;; specify browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program
      (--first
       (executable-find it)
       '("chromium-browser"
         "google-chrome"
         "google-chrome-stable"
         "google-chrome-beta"
         "firefox")))

;; Set default browser
(setq browse-url-browser-function 'browse-url-chromium)

(leaf bind-key
  :ensure t
  :config
  ;; disable dangerous keys
  (bind-keys
   ("C-x C-c" . nil)
   ("C-x C-z" . nil))
  ;; set key binds
  (bind-key "C-h" 'delete-backward-char))

;;; C-x C-c -> "exit" command
(defalias 'exit 'save-buffers-kill-emacs)

;; easy to descern buffers of same name files
(leaf uniquify
  :require t
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

;; emacs server
(leaf server
  :require t
  :config
  (unless (server-running-p)
    (server-start)))

(leaf ido-vertical-mode
  :ensure t
  :config
  (progn
    (setq ido-max-window-height 0.75)
    (setq ido-enable-flex-matching t)
    (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
    (ido-vertical-mode 1)))

;; ivy/counsel settings
(leaf ivy
  :ensure t
  :diminish (ivy-mode . "🅘")
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(defun my-counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "This is a counsel-rg alternative. It searches a text in the current directory.
It you need to search a text in a project root directory,
use projectile-counsel-rg instead."
  (interactive)
  (let ((counsel-ag-base-command
         (if (listp counsel-rg-base-command)
             (append counsel-rg-base-command (counsel--rg-targets))
           (concat counsel-rg-base-command " "
                   (mapconcat #'shell-quote-argument (counsel--rg-targets) " "))))
        (counsel--grep-tool-look-around
         (let ((rg (car (if (listp counsel-rg-base-command) counsel-rg-base-command
                          (split-string counsel-rg-base-command))))
               (switch "--pcre2"))
           (and (eq 0 (call-process rg nil nil nil switch "--pcre2-version"))
                switch))))
    (counsel-ag initial-input default-directory extra-rg-args rg-prompt
                :caller 'counsel-rg)))

;; How to edit the result lines
;;
;; 1. search with swiper, counsel-rg, etc.
;; 2. (optional) ivy-avy (C-') : search a target
;; 3. ivy-occur (C-c C-o) : start occur to edit
;; 4. ivy-wgrep-change-to-wgrep-mode (C-x C-q) : start editing
;; 5. wgrep-finish-edit (C-c C-c) : commit
;;
;; How to change counsel-rg directory
;;
;; 1. search with cousel-rg which searches in a project root directory
;; 2. counsel-cd (C-x C-d)
(leaf counsel
  :ensure t
  :after (ivy)
  :bind (("C-c h" . counsel-recentf)
         ("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-S-f" . my-counsel-rg)
         ("C-x C-f" . counsel-find-file)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-b" . counsel-ibuffer))
  :config
  ;; The following glob setting doesn't work. Maybe it should be invoked later
  ;; (push '(counsel-rg . "--glob '**' -- ") ivy-initial-inputs-alist)
  (ivy-set-actions
   'my-counsel-rg
   '(("j" counsel-find-library-other-window "other window")
     ("f" counsel-find-library-other-frame "other frame")))
  :custom
  `((counsel-yank-pop-separator . "\n――――――――\n")))

(leaf swiper
  :ensure t
  :after (ivy))

(leaf all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup)
  (setq all-the-icons-ivy-file-commands
        '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir)))

(leaf ivy-posframe
  :ensure t
  :after (ivy)
  :diminish
  :config
  (setq ivy-posframe-display-functions-alist
        '((swiper . nil)
          (t      . ivy-posframe-display-at-window-center)))
  (ivy-posframe-mode 1))

(leaf ivy-prescient
  :ensure t
  :after (ivy)
  :config
  (ivy-prescient-mode))

(defun counsel-ghq (&optional initial-input)
  "Open a file using the ghq shell command."
  (interactive)
  (let ((candidates (split-string
                     (shell-command-to-string
                      "ghq list --full-path")
                     "\n")))
    (ivy-read "ghq: "
              candidates
              :initial-input initial-input
              :action #'find-file
              :caller 'counsel-ghq)))

;; edit grep result directry
(leaf wgrep
  :ensure t)

;; edit dired result directory
(leaf wdired
  :ensure t
  :config
  (bind-key "C-c C-e" 'wdired-change-to-wdired-mode dired-mode-map))

;; prescient
(leaf prescient
  :ensure t
  :config
  (prescient-persist-mode))

;; comapny
(leaf company
  :ensure t
  :diminish (company-mode . "🅒")
  :config
  (global-company-mode)
  (progn
    (setq company-idle-delay 0.1
          company-minimum-prefix-length 2
          company-selection-wrap-around t)
    (setq company-dabbrev-downcase nil))

  (bind-keys :map company-mode-map
             ("C-i" . company-complete))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-search-words-regexp))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))

(leaf company-box
  :ensure t
  :diminish
  :hook
  (company-mode . company-box-mode))

(leaf company-prescient
  :ensure t
  :after (company)
  :config
  (company-prescient-mode))

;; undo-tree.el
(leaf undo-tree
  :ensure t
  :diminish (undo-tree-mode . "🅤")
  :config
  (global-undo-tree-mode))

;; migemo
(leaf migemo
  :ensure t
  :commands migemo
  :config
  (progn
    (setq migemo-command (if (executable-find "cmigemo") "cmigemo" "/usr/local/bin/cmigemo"))
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary
          (--find
           (f-exists? it)
           '("/usr/share/migemo/utf-8/migemo-dict"
             "/usr/share/cmigemo/utf-8/migemo-dict"
             "/usr/local/share/migemo/utf-8/migemo-dict")))
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (load-library "migemo")
    (migemo-init)))

;; flycheck
(leaf flycheck
  :diminish (flycheck-mode . "⚠")
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq spaceline-flycheck-bullet "⚠%s"))

(leaf flycheck-posframe
  :ensure t
  :after (flycheck)
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; junk file
(leaf open-junk-file
  :ensure t
  :commands open-junk-file
  :config
  ;; open junk file in a current window
  (setq open-junk-file-find-file-function 'find-file))

;; magit.el
(leaf magit
  :ensure t
  :commands magit
  ;; same as IntelliJ IDEA short cut
  :bind (("M-9" . magit-status))
  :config
  (progn
    (add-hook 'magit-status-mode-hook (lambda ()
                                        (company-mode -1)))
    (setq magit-diff-refine-hunk t)))

;; quickrun.el
(leaf quickrun
  :ensure t
  :commands quickrun
  :bind (([shift f5] . quickrun)))

;; shell-pop.el
(leaf shell-pop
  :ensure t
  :commands shell-pop
  ;; same as IntelliJ IDEA short cut
  :bind (([M-f12] . shell-pop)))

;; treemacs
(leaf treemacs
  :ensure t
  :leaf-defer t
  :config
  (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       t
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-desc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)
  ;; FIXME
  ;; (treemacs-follow-mode t)
  ;; (treemacs-filewatch-mode t)
  ;; (treemacs-fringe-indicator-mode t)
  ;; (pcase (cons (not (null (executable-find "git")))
  ;;              (not (null (executable-find "python3"))))
  ;;   (`(t . t)
  ;;    (treemacs-git-mode 'deferred))
  ;;   (`(t . _)
  ;;    (treemacs-git-mode 'simple)))
  ;; FIXME
  ;; :bind
  ;; (:map global-map
  ;;       ;; same as IntelliJ IDEA short cut
  ;;       ("M-1" . treemacs))
  )

(leaf treemacs-projectile
  :ensure t
  :after treemacs projectile)

(leaf treemacs-icons-dired
  :ensure t
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(leaf treemacs-magit
  :ensure t
  :after treemacs magit)

;; multiple-cursors and enhancers
(leaf multiple-cursors
  :ensure t
  :bind (([C-M-return] . mc/edit-lines)))

(leaf expand-region
  :ensure t
  :commands expand-region
  :bind (("C-,"   . er/expand-region)
         ("C-M-," . er/contract-region)))

(leaf smartrep
  :ensure t
  :config
  (smartrep-define-key
      global-map "C-." '(("n" . 'mc/mark-next-like-this)
                         ("p" . 'mc/mark-previous-like-this)
                         ("P" . 'mc/unmark-next-like-this)
                         ("N" . 'mc/unmark-previous-like-this)
                         ("*" . 'mc/mark-all-like-this))))

;; anzu
(leaf anzu
  :ensure t
  :diminish (anzu-mode . "🅐")
  :config
  (global-anzu-mode +1)
  (setq anzu-cons-mode-line-p nil))

;; ace-isearch
(leaf ace-isearch
  :ensure t
  :diminish ace-isearch-mode
  :config
  (global-ace-isearch-mode 1)
  (setq ace-isearch-function 'avy-goto-char)
  (setq ace-isearch-function-from-isearch 'ace-isearch-swiper-from-isearch))

(leaf ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-x o") 'other-window-or-split)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; FIXME
  ;; :custom-face
  ;; (aw-leading-char-face . ((t (:height 4.0 :foreground "#f1fa8c"))))
  )

;; ace-window
;; w/ http://d.hatena.ne.jp/rubikitch/20100210/emacs
(defun other-window-or-split ()
  (interactive)
  (if (one-window-p)
      (split-window-horizontally)
    (ace-window 1)))

;; visual-regexp-steroids
(leaf visual-regexp-steroids
  :ensure t)

;; electric-pair-mode
(electric-pair-mode t)

(leaf wrap-region
  :ensure t
  :config
  (wrap-region-mode t))

(leaf direnv
  :ensure t
  :config
  (direnv-mode))

;; ELDoc
(leaf eldoc
  :ensure t
  :diminish (eldoc-mode . "📖"))

;; shackle
;; popup interface
(leaf shackle
  :ensure t
  :config
  (shackle-mode 1)
  (setq shackle-default-rule '(:other t))
  (setq shackle-rules '(("*undo-tree*"         :regexp t :align right :size 0.25)
                        ("*Backtrace*"         :regexp t :align t     :size 0.4)
                        ("*Warnings*"          :regexp t :align t     :size 0.4)
                        ("*cider-error*"    :inhibit-window-quit t)
                        ("\\`\\*magit.*?\\*\\'" :regexp t :select t   :inhibit-window-quit t :same t)
                        )))

;; which-key
(leaf which-key
  :ensure t
  :config
  (which-key-mode))

;; spelling
;; requires packages
;;   * hunspell
;;   * hunspell-en_US
(leaf ispell
  :ensure t
  :config
  (setq ispell-program-name (executable-find "hunspell")
        ispell-dictionary "en_US-large"
        ispell-really-hunspell t))

;; The following lines are hunspell settings with Japanese.
;; See: https://www.emacswiki.org/emacs/FlySpell#toc14
;; Because an aspell setting
;;   (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
;; does not work!
(defun my-flyspell-ignore-non-ascii (beg end info)
  "Tell flyspell to ignore non ascii characters.
  Call this on `flyspell-incorrect-hook'."
  (string-match "[^!-~]" (buffer-substring beg end)))

(leaf flyspell
  :ensure t
  :hook
  (flyspell-incorrect-hook . my-flyspell-ignore-non-ascii)
  :config
  ;; ignore errors on macOS
  (setq flyspell-issue-message-flag nil)
  ;; They conflict with expand-region's bindings
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (add-hook 'text-mode-hook 'flyspell-mode)
  (add-hook 'prog-mode-hook 'flyspell-prog-mode))

;; google-translate
(leaf google-translate
  :ensure t
  :after (posframe)
  :config
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja"))))

;; comment-dwim-2
(leaf comment-dwim-2
  :ensure t
  :bind
  (("M-;" . comment-dwim-2)))

(leaf dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "(start Emacs)")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((agenda . 5)
                          (recents  . 5)
                          (projects . 5)
                          (registers . 5))))

;; basic theme settings
(leaf atom-one-dark-theme
  :ensure t
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
(defcustom my-font
  (let* ((fonts (font-family-list))
         (available (-find
                     (lambda (f) (when (-contains? fonts f) f))
                     '("ricty discord nerd font"
                       "Ricty Discord Nerd Font"
                       "ricty discord"
                       "Ricty Discord"
                       "ricty nerd font"
                       "Ricty Nerd Font"
                       "ricty"
                       "Ricty")))
         (size 14))
    (format "%s-%d" available size))
  "Font. It's detected automaticaly by default.")

(setq default-frame-alist
      (list (cons 'font  my-font)))
(set-frame-font my-font)

;; set color, window size
(if window-system
    (progn
      (set-frame-parameter nil 'alpha 95)))

;; line number
(global-linum-mode 1)
(setq linum-format "%4d.")

;; show spaces
(leaf whitespace
  :ensure t
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
(leaf git-gutter-fringe
  :ensure t
  :diminish git-gutter-mode
  :config
  (progn
    (global-git-gutter-mode)
    (setq git-gutter-fr:side 'right-fringe)))

;; rainbow-delimiters
(leaf rainbow-delimiters
  :ensure t)

;; highlight indentation
(leaf highlight-indentation
  :ensure t
  :diminish highlight-indentation-mode)

;; mode line
(leaf doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 1))

;; volatile-highlights
(leaf volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

;; pretty-symbols
(global-prettify-symbols-mode +1)

;; diminish
(leaf diminish
  :ensure t
  :config
  (diminish 'auto-revert-mode "⟳")
  (diminish 'view-mode "👁"))


;; hide-modeline
(leaf hide-mode-line
  :ensure t
  :hook
  ((treemacs-mode) . hide-mode-line-mode))

(leaf alert
  :ensure t
  :config
  (setq alert-default-style 'libnotify))

(defcustom my-skk-jisyo-root
  (-find 'f-directory? '("/usr/share/skk" "~/skk"))
  "SKK dictionary path. Override it for each platform")

(leaf ddskk
  :ensure t
  :bind (("C-o"     . skk-mode)
         ("C-x C-j" . skk-mode))
  :config
  ;; enable AZIK
  (setq skk-use-azik t)
  ;; disable skk-isearch for migemo
  (setq skk-isearch-mode-enable nil)
  (setq skk-search-start-mode 'latin)
  ;; candidates position
  (setq skk-show-tooltip t)
  ;; dynamic completion
  ;;   てん -> てんさい, てんしょく、てんかん
  (setq skk-dcomp-activate t)
  (setq skk-dcomp-multiple-activate t)
  (setq skk-dcomp-multiple-rows 10)
  ;; dictionary
  (setq skk-large-jisyo (concat my-skk-jisyo-root "/SKK-JISYO.L"))
  (setq skk-extra-jisyo-file-list
        (--map (concat my-skk-jisyo-root it)
               '("/SKK-JISYO.geo"
                 "/SKK-JISYO.jinmei"
                 "/SKK-JISYO.propernoun"
                 "/SKK-JISYO.station"))))

(leaf ddskk-posframe
  :ensure t
  :diminish
  :config
  (ddskk-posframe-mode t))
