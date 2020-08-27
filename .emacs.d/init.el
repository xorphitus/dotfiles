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

(leaf dash
  :tag "library"
  :ensure t
  :require t)
(leaf drag-stuff
  :tag "library"
  :ensure t
  :require t)
(leaf s
  :tag "library"
  :ensure t
  :require t)
(leaf f
  :tag "library"
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

(leaf uniquify
  :doc "Easy to descern buffers of same name files"
  :require t
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(leaf server
  :doc "Emacs server"
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

(leaf counsel
  :doc
  "How to edit the result lines
 1. search with swiper, counsel-rg, etc.
 2. (optional) ivy-avy (C-') : search a target
 3. ivy-occur (C-c C-o) : start occur to edit
 4. ivy-wgrep-change-to-wgrep-mode (C-x C-q) : start editing
 5. wgrep-finish-edit (C-c C-c) : commit

 How to change counsel-rg directory

 1. search with cousel-rg which searches in a project root directory
 2. counsel-cd (C-x C-d)"
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

(leaf wgrep
  :doc "Edit grep result directry"
  :ensure t)

(leaf wdired
  :doc "Edit dired result directory"
  :ensure t
  :config
  (bind-key "C-c C-e" 'wdired-change-to-wdired-mode dired-mode-map))

(leaf prescient
  :ensure t
  :config
  (prescient-persist-mode))

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

(leaf undo-tree
  :ensure t
  :diminish (undo-tree-mode . "🅤")
  :config
  (global-undo-tree-mode))

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

(leaf open-junk-file
  :ensure t
  :commands open-junk-file
  :config
  ;; open junk file in a current window
  (setq open-junk-file-find-file-function 'find-file))

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

(leaf multiple-cursors
  :ensure t
  :bind (([C-M-return] . mc/edit-lines)))

(leaf expand-region
  :doc "multiple-cursors enhancer"
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

(leaf anzu
  :ensure t
  :diminish (anzu-mode . "🅐")
  :config
  (global-anzu-mode +1)
  (setq anzu-cons-mode-line-p nil))

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

(defun other-window-or-split ()
  "for ace-window
See http://d.hatena.ne.jp/rubikitch/20100210/emacs"
  (interactive)
  (if (one-window-p)
      (split-window-horizontally)
    (ace-window 1)))

(leaf visual-regexp-steroids
  :ensure t)

(electric-pair-mode t)

(leaf wrap-region
  :ensure t
  :config
  (wrap-region-mode t))

(leaf direnv
  :ensure t
  :config
  (direnv-mode))

(leaf eldoc
  :ensure t
  :diminish (eldoc-mode . "📖"))

(leaf shackle
  :doc "Popup interface"
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

(leaf which-key
  :ensure t
  :config
  (which-key-mode))

(leaf ispell
  :req "hunspell" "hunspell-en_US"
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

(leaf google-translate
  :ensure t
  :after (posframe)
  :config
  (setq google-translate-translation-directions-alist
        '(("ja" . "en") ("en" . "ja"))))

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

(leaf atom-one-dark-theme
  :doc "Basic theme settings"
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

(leaf whitespace
  :doc "Show spaces"
  :ensure t
  :diminish (global-whitespace-mode . "🅦")
  :commands whitespace
  :init
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
                      :weight 'bold))

;; show an icon indicating whether a line has been changed
;; from last commit
;; FIXME it makes display wrong!
;; (leaf git-gutter-fringe
;;   :ensure t
;;   :diminish git-gutter-mode
;;   :config
;;   (progn
;;     (global-git-gutter-mode)
;;     (setq git-gutter-fr:side 'right-fringe)))

(leaf rainbow-delimiters
  :ensure t)

(leaf highlight-indentation
  :ensure t
  :diminish highlight-indentation-mode)

(leaf doom-modeline
  :ensure t
  :hook
  (after-init . doom-modeline-mode)
  :config
  (line-number-mode 0)
  (column-number-mode 1))

(leaf volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(global-prettify-symbols-mode +1)

(leaf diminish
  :ensure t
  :config
  (diminish 'auto-revert-mode "⟳")
  (diminish 'view-mode "👁"))

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

(leaf dumb-jump
  :ensure t
  :config
  (dumb-jump-mode))

(leaf counsel-gtags
  :ensure t)

(defun my-smart-jump-configuration-with-gtags (modes)
  "smart-jump helper function"
  (progn
    (smart-jump-register :modes modes
                         :jump-fn 'counsel-gtags-find-definition)
    (smart-jump-register :modes modes
                         :jump-fn 'xref-find-definitions)))

(leaf smart-jump
  :ensure t
  :config
  (smart-jump-setup-default-registers)
  ;; xref config
  ;; eglot uses xref: it means that no special configurations are needed for language servers
  (smart-jump-register :modes '(shell-mode
                                haskell-mode
                                rust-mode))
  ;; xref (lsp) -> gtags config
  (my-smart-jump-configuration-with-gtags '(c-mode-hook
                                            c++-mode-hook
                                            lisp-mode-hook
                                            ruby-mode-hook
                                            js2-mode-hook
                                            python-mode-hook
                                            php-mode-hook)))

(leaf eglot
  :ensure t
  ;:hook
  ;(eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  )

;; The following is copied from
;; https://github.com/akash-akya/emacs-flymake-cursor
;; The original flymake-cursor on MELPA is deprecated now
(defgroup flymake-cursor nil
  "Show flymake errors for current line in message area."
   :group 'tools)

(defcustom flymake-cursor-error-display-delay 0.9
  "Delay in seconds to wait before displaying flymake errors for the current line."
  :group 'flymake-cursor
  :type 'number)

(defcustom flymake-cursor-number-of-errors-to-display 1
  "Number of flymake errors to display if there are more than one.
If set to nil, all errors for the line will be displayed.
If there are more errors than can be displayed in the minibuffer, the
first ones will be scrolled off. You will probably want to set this
variable to a value consistent with your `max-mini-window-height'
setting."
  :group 'flymake-cursor
  :type '(choice integer (const nil)))

(defcustom flymake-cursor-auto-enable t
  "Whether flymake-cursor should automatically enable itself whenever
flymake is enabled.
If set to t, flymake-cursor will turn on whenever flymake does.
If set to nil, flymake-cursor will need to be manually enabled.
Regardless of this setting, flymake-cursor will always disable
itself automatically when flymake is disabled, to prevent
errors."
  :group 'flymake-cursor
  :type 'boolean)

(defvar flymake-cursor-errors-at-point nil
  "Errors at point, after last command")

(defvar flymake-cursor-error-display-timer nil
  "A timer; when it fires, it displays the stored error message.")

;;;###autoload
(define-minor-mode flymake-cursor-mode
  "Minor mode to show `flymake-mode' errors for the current line in the
message area.
When called interactively, toggles the minor mode.
With arg, turn Flymake Cursor mode on if and only if arg is positive.
Usually `flymake-cursor-mode' is enabled and disabled automatically with
`flymake-mode' for the current buffer and you will not need to toggle
the mode directly."
  :group 'flymake-cursor
  (cond

    ;; Turning the mode ON.
    (flymake-cursor-mode
      (add-hook 'post-command-hook 'flymake-cursor-show-errors-at-point-pretty-soon nil t))
    ;; Turning the mode OFF.
    (t
      (flymake-cursor-cancel-error-display-timer)
      (remove-hook 'post-command-hook 'flymake-cursor-show-errors-at-point-pretty-soon t))))

(defun flymake-cursor-get-errors-at-point ()
  "Gets the first `flymake-cursor-number-of-errors-to-display` flymake errors on the line at point."
  (let ((line-err-info-list (flymake-cursor-get-errors)))
    (if flymake-cursor-number-of-errors-to-display
        (butlast line-err-info-list (- (length line-err-info-list) flymake-cursor-number-of-errors-to-display))
      line-err-info-list)))

(defun flymake-cursor-get-errors ()
  (cond ((boundp 'flymake-err-info)  ; emacs < 26
         (let ((lineno (line-number-at-pos)))
           (car (flymake-find-err-info flymake-err-info lineno))))
        ((and (fboundp 'flymake-diagnostic-text)
              (fboundp 'flymake-diagnostics))  ; emacs >= 26
         (flymake-diagnostics (point)))))

(defun flymake-cursor-pyflake-determine-message (error)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
        ((fboundp 'flymake-diagnostic-text)
         (let ((msg (flymake-diagnostic-text error)))
           (if (null msg) msg (format "compile error, problem on line %s" msg))))
        (t
         (flymake-ler-text error))))

(defun flymake-cursor-safe-to-display ()
  "Returns t if Flymake Cursor is safe to display to the minibuffer or nil if
something else is using the message area."
  ;;  Don't trash the minibuffer while they're being asked a question.
  (not (or (active-minibuffer-window) cursor-in-echo-area)))

(defun flymake-cursor-show-stored-errors-now ()
  "Displays the stored error in the minibuffer."
  (interactive)
  (when flymake-cursor-mode
    (flymake-cursor-cancel-error-display-timer)
    (when flymake-cursor-errors-at-point
      (if (flymake-cursor-safe-to-display)
        (message "%s" (mapconcat 'flymake-cursor-pyflake-determine-message flymake-cursor-errors-at-point "\n"))
        (flymake-cursor-show-errors-at-point-pretty-soon)))))

(defun flymake-cursor-show-errors-at-point-now ()
  "If the cursor is sitting on a flymake error, display
the error message in the minibuffer."
  (interactive)
  (when flymake-cursor-mode
    (flymake-cursor-cancel-error-display-timer)
    (setq flymake-cursor-errors-at-point (flymake-cursor-get-errors-at-point))
    (if flymake-cursor-errors-at-point
      (flymake-cursor-show-stored-errors-now)
      ;; If something is demanding we display errors immediately, we do
      ;; want to clear the message area to indicate there's no errors.
      ;; Otherwise flymake-cursor-after-syntax-check will just keep the
      ;; old error for the current line if it has been corrected.
      (when (flymake-cursor-safe-to-display)
        (message nil)))))

(defun flymake-cursor-cancel-error-display-timer ()
  "Cancels `flymake-cursor-error-display-timer'."
  (when flymake-cursor-error-display-timer
    (cancel-timer flymake-cursor-error-display-timer)
    (setq flymake-cursor-error-display-timer nil)))

(defun flymake-cursor-show-errors-at-point-pretty-soon ()
  "If the cursor is sitting on a flymake error, grab the error,
and set a timer for \"pretty soon\". When the timer fires, the error
message will be displayed in the minibuffer.
The interval before the timer fires can be customized in the variable
`flymake-cursor-error-display-delay'.
This allows a post-command-hook to NOT cause the minibuffer to be
updated 10,000 times as a user scrolls through a buffer
quickly. Only when the user pauses on a line for more than a
second, does the flymake error message (if any) get displayed."
  (flymake-cursor-cancel-error-display-timer)
  (setq flymake-cursor-errors-at-point (flymake-cursor-get-errors-at-point))
  (when flymake-cursor-errors-at-point
    (setq flymake-cursor-error-display-timer
      (run-at-time flymake-cursor-error-display-delay nil 'flymake-cursor-show-stored-errors-now))))

(defun flymake-cursor-follow-flymake-mode ()
  "Hook function to make `flymake-cursor-mode` follow the on/off
status of `flymake-mode'."
  (if flymake-mode
    (when flymake-cursor-auto-enable (flymake-cursor-mode 1))
    (flymake-cursor-mode 0)))

(defun flymake-cursor-after-syntax-check ()
  "Run from `flymake-after-syntax-check-hook' to update our errors."
  (when (eq (current-buffer) (window-buffer))
    (flymake-cursor-show-errors-at-point-now)))

(eval-after-load "flymake"
  '(progn
    (if (boundp 'flymake-goto-error-hook)
      (add-hook 'flymake-goto-error-hook 'flymake-cursor-show-errors-at-point-now)
      (defadvice flymake-goto-line (after flymake-cursor-display-message-after-move-to-error activate compile)
        "Display the error in the mini-buffer rather than having to mouse over it"
         (flymake-cursor-show-errors-at-point-now)))
    (if (boundp 'flymake-after-syntax-check-hook)
      (add-hook 'flymake-after-syntax-check-hook 'flymake-cursor-after-syntax-check)
      (defadvice flymake-post-syntax-check (after flymake-cursor-display-message-after-syntax-check activate compile)
        "Display the error in the mini-buffer rather than having to mouse over it"
        (flymake-cursor-after-syntax-check)))
    (add-hook 'flymake-mode-hook 'flymake-cursor-follow-flymake-mode)))

(leaf flymake
  :ensure t
  :after
  (eglot)
  :config
  (flymake-cursor-mode))

(leaf paredit
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'common-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode))

(defun my-auto-set-projectile-root-to-eyebrowse ()
  "Rename workspace names automaticaly!"
  (ignore-errors
    (let ((current-root "TODO: get from eyebrowse")
          (projectile-root (-> (projectile-project-info)
                               (split-string " ## ")
                               (car)
                               (split-string ": ")
                               (last)
                               (car))))
      (when (not (string= (replace-regexp-in-string "/$" "" projectile-root) current-root))
        (let ((new-name (-> projectile-root
                            (split-string "/")
                            ((lambda (lst) (--remove (string= it "") lst)))
                            (last)
                            (car))))
          (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) new-name))))))

(leaf projectile
  :ensure t
  :commands projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " 📁 %s" (projectile-project-name)))))

(leaf counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))


(leaf slime
  :ensure t
  :init
  ;; require SBCL
  (setq inferior-lisp-program "sbcl")
  :ensure t
  :config
  (slime-setup '(slime-repl slime-fancy slime-banner))
  (slime-setup '(slime-fancy slime-company)))

(leaf yasnippet
  :ensure t
  :diminish (yas-minor-mode . "🅨")
  :config
  (yas-global-mode 1)
  (bind-key "M-=" 'yas-insert-snippet yas-minor-mode-map))

(defun add-c-hooks (hook)
  "add hook only c-mode-hook, c++-mode-hook. since c-mode-common-hook includes others hooks"
  (add-hook 'c-mode-hook hook)
  (add-hook 'c++-mode-hook hook))

;; flycheck
(add-hook 'c-mode-common-hook 'flycheck-mode)

;; compile C-c
(add-hook 'c-mode-common-hook
          '(lambda ()
             (define-key mode-specific-map "c" 'compile)))

(leaf google-c-style
  :ensure t
  :commands google-c-style
  :init
  (progn
    (add-c-hooks 'google-set-c-style)
    (add-c-hooks 'google-make-newline-indent)))

;; GDB
(setq gdb-many-windows t)
;; show value of variable when mouse cursor on
(add-hook 'gdb-mode-hook '(lambda () (gud-tooltip-mode t)))
;; show I/O buffer
(setq gdb-use-separate-io-buffer t)

(leaf cider
  :ensure t
  :init
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-mode-hook #'clj-refactor-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  :config
  ;; Hide the *nrepl-connection* and *nrepl-server* buffers
  ;; from appearing in some buffer switching commands like 'C-x b'
  (setq nrepl-hide-special-buffers t)
  ;; The REPL buffer name  will look like cider project-name:port
  (setq nrepl-buffer-name-show-port t)
  ;; Enable CIDER and figwheel interaction
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
               (figwheel-sidecar.repl-api/start-figwheel!)
               (figwheel-sidecar.repl-api/cljs-repl))"))

(leaf clj-refactor
  ensure t
  :config
  (progn
    (clj-refactor-mode 1)
    (yas-minor-mode 1)
    (cljr-add-keybindings-with-prefix "C-c j")))

;;; compojure indentation
(add-hook 'clojure-mode-hook
          (lambda()
            (define-clojure-indent
              (defroutes 'defun)
              (GET 2)
              (POST 2)
              (PUT 2)
              (DELETE 2)
              (HEAD 2)
              (ANY 2)
              (context 2))))

(leaf compile
  :doc "Get kibit output"
  :ensure t
  :commands compile
  :init
  (progn
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))))

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

(defun kibit-current-file ()
  "Run kibit on the current file.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile (concat "lein kibit " buffer-file-name)))

;; rainbow delimiters
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook 'highlight-indentation-mode)

;; prittify symbols
(my-macro/prettify-symbols
 clojure-mode-hook
 (-concat '(("fn" . ?λ))
          my-const/logical-prettify-symbols-alist
          my-const/relational-prettify-symbols-alist))

(leaf dockerfile-mode
  :ensure t
  :mode (("Dockerfile" . dockerfile-mode)))

(add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
(add-hook 'emacs-lisp-mode-hook 'highlight-indentation-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(defconst lisp--prettify-symbols-alist
  (-concat my-const/lambda-prettify-symbols-alist
           my-const/logical-prettify-symbols-alist
           my-const/relational-prettify-symbols-alist))

(leaf go-mode
  :ensure t
  :commands go-mode
  :init
  ;; flycheck
  (add-hook 'go-mode-hook 'flycheck-mode)
  ;; indentation
  (add-hook 'go-mode-hook 'highlight-indentation-mode)
  (add-hook 'go-mode-hook (lambda ()
                            (setq tab-width 4)))
  ;; prittify symbols
  (my-macro/prettify-symbols
     go-mode-hook
     my-const/relational-prettify-symbols-alist))

(leaf haskell-mode
  :ensure t
  :mode ((".xmobarrc" . haskell-mode))
  :init
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(leaf markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (put 'dired-find-alternate-file 'disabled nil))

;;; org-mode

(setq org-agenda-files (list "~/Documents/org"))

;; It conflicts with expand-region's bindings
(leaf org-mode
  :mode (("\\.org$" . org-mode))
  :bind
  (("C-," . nil)))

;; beautify org-mode list bullets
;; FIXME doesn't work!
(leaf org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :custom
  (org-bullets-bullet-list '("🌕" "🌔" "🌓" "🌒" "🌑")))

;; babel
(setq org-plantuml-jar-path
      (--find
       (f-exists? it)
       '("/usr/share/java/plantuml/plantuml.jar")))

;; How to use org-babel
;; The following is an example for PlantUML
;;
;;   #+BEGIN_SRC plantuml :file example.png :cmdline -charset UTF-8
;;   animal <|-- sheep
;;   #+END_SRC
;;
;; Then, type "C-c C-c" inside BEGIN_SRC ~ END_SRC
;; It creates an image file.
;; To show the image file inline, use the following.
;;   org-toggle-inline-images (C-c C-x C-v)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)))

(defun sound-wav--do-play (files)
  "Need to override this function
since https://github.com/syohex/emacs-sound-wav which is depended by org-pomodoro
does not support PulseAudio's pacat/paplay"
  (let* ((vol-percent 75)
         (vol (round (/ (* 65536 vol-percent) 100))))
    (if (executable-find "afplay")
        ;; for macOS
        (sound-wav--do-play-by-afplay files)
      ;; for PulseAudio
      (deferred:$
        (apply 'deferred:process org-pomodoro-audio-player (concat "--volume=" (number-to-string vol)) files)))))

(leaf org-pomodoro
  :ensure t
  :after
  org-agenda
  :custom
  `((org-pomodoro-audio-player . ,(or (executable-find "pacat")
                                     (executable-find "paplay")
                                     (executable-find "aplay")
                                     (executable-find "afplay")))
    (org-pomodoro-format . "🍅%s")
    (org-pomodoro-short-break-format . "☕%s")
    (org-pomodoro-long-break-format . "🌴%s")))

(leaf org-alert
  :doc "Set alerts for scheduled tasks"
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-alert-enable))))

(leaf shell-script-mode
  :mode (("PKGBUILD" . shell-script-mode)
         ("\\.install$" . shell-script-mode)))

(setq sh-basic-offset 2)
(setq sh-indentation 2)

(leaf fish-mode
  :ensure t
  :custom
  (fish-indent-offset . 2))

;; UTF-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)

;;; init.el ends here
