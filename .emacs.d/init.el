;;; init.el --- xorphitus elisp

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This is an entry point of xorphitus elisp.

;;; Code:

(setq gc-cons-threshold (* 128 1024 1024))

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
     'package-archives '(("melpa" . "https://melpa.org/packages/")
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

(leaf el-get
  :doc "The primary option is MELPA, however rarely it doesn't distribute a required package. Therefore el-get is enabled for such a case"
  :ensure t
  :init
  (add-to-list 'load-path "~/.emacs.d/el-get/el-get"))

(leaf esup
  :doc "Enable this package when I want to measure performance"
  :disabled t
  :ensure t)

(leaf *library
  :config
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
  (leaf diminish
    :doc "This is not a library, but it's required for leaf DSL"
    :ensure t
    :require t))

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

(defconst my-const/arrow-prettify-symbols-alist
  '(("->" . ?→)
    ("=>" . ?⇒)))

;; Macros

;; Somehow, this macro causes an issue that Emacs suddenly doesn't accept keyboard inputs.
;; Stop using this one for now.
(defmacro skip--my-macro/prettify-symbols (hook symbols-alist)
  `(add-hook ',hook
             (lambda ()
               (-each ,symbols-alist
                 (lambda (prettify-map)
                   (push prettify-map prettify-symbols-alist))))))
(defmacro my-macro/prettify-symbols (hook symbols-alist))

(leaf *base
  :config
  (setq
   ;; Use sshx instead of ssh in tramp
   tramp-default-method "sshx"
   ;; show explicit file name
   explicit-shell-file-name shell-file-name
   ;; file name completion ignore case
   completion-ignore-case t
   read-file-name-completion-ignore-case t
   ;; without backup-file
   backup-inhibited t
   ;; delete auto save files when quit
   delete-auto-save-files t
   ;; well, I'd like to avoid auto save files themselves since I don't use them
   auto-save-default nil
   ;; disable beep sound flash
   ring-bell-function 'ignore
   ;; For time locale for org-mode to avoid Japanese time locale format
   ;; However this is not an org-mode specific setting but a global setting, written here
   system-time-locale "C"
   ;; tab-width
   default-tab-width 4
   ;; specify browser
   browse-url-browser-function 'browse-url-generic
   browse-url-generic-program (--first
                               (executable-find it)
                               '("vivaldi"
                                 "vivaldi-stable"
                                 "chromium-browser"
                                 "google-chrome"
                                 "google-chrome-stable"
                                 "google-chrome-beta"
                                 "firefox")))
  ;; yes/no -> y/n
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; indent
  (setq-default indent-tabs-mode  nil)
  ;; set close paren automatically
  (electric-pair-mode t))

(leaf *completion
  :config
  (leaf vertico
    :ensure t
    :init
    (vertico-mode))

  (leaf marginalia
    :ensure t
    :init
    ;; Must be in the :init section
    (marginalia-mode)
    :config
    ;; Categorize org-roam functions. It enables migemo
    ;; to the command. See the orderless setting which
    ;; enables migemo to some categories.
    (add-to-list 'marginalia-command-categories
                 '(org-roam-node-find . unicode-name))
    (add-to-list 'marginalia-command-categories
                 '(org-roam-node-insert . unicode-name)))

  (leaf orderless
    :ensure t
    :init
    ;; TODO I really wanna remove this require, cuz I'm using leaf.
    ;; But somehow I failed to call orderless-define-completion-style
    ;; without this require.
    (require 'orderless)
    (defun my-orderless-migemo (component)
      "Helper function to enable migemo to Vertico via Orderless"
      (let ((pattern (migemo-get-pattern component)))
        (condition-case nil
            (progn (string-match-p pattern "") pattern)
          (invalid-regexp nil))))

    :config
    (orderless-define-completion-style orderless-migemo-style
      (orderless-matching-styles '(; orderless-literal
                                   ; orderless-regexp
                                   my-orderless-migemo)))

    (setq completion-styles '(orderless)
          completion-category-defaults nil
          completion-category-overrides
          '((file (styles orderless-migemo-style))
            (buffer (styles orderless-migemo-style))
            (consult-location (styles orderless-migemo-style))
            (consult-multi (styles orderless-migemo-style))
            (unicode-name (styles orderless-migemo-style)))))

  (leaf consult
    :ensure t
    :bind
    (("M-y" . consult-yank-from-kill-ring)
     ("C-x b" . consult-buffer)
     ("C-c h" . consult-recent-file))

    ;; Enable automatic preview at point in the *Completions* buffer.
    ;; This is relevant when you use the default completion UI,
    ;; and not necessary for Selectrum, Vertico etc.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     :preview-key '(:debounce 0.4 any))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; Optionally configure a function which returns the project root directory.
    ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
    (setq consult-project-root-function
          (lambda ()
            (when-let (project (project-current))
              (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
    ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
    ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
    )

  (leaf consult-ghq
    :ensure t
    :config
    (setq consult-ghq-find-function #'consult-find))

  (leaf embark
    :ensure t

    :bind
    (("C-'" . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

    :init
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)

    :config
    ;; Hide the mode line of the Embark live/completions buffers
    (add-to-list 'display-buffer-alist
                 '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                   nil
                   (window-parameters (mode-line-format . none)))))

  (leaf embark-consult
    :ensure t
    :after (embark consult)
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))

(leaf autorevert
  :doc "Auto reload buffer which modified by external programs"
  :tag "builtin"
  :global-minor-mode global-auto-revert-mode)

(leaf cua
  :doc "Rectanble select
GUI: C-Ret
CUI: 'cua-set-rectangle-mark
M-n -> insert numbers incremental"
  :tag "builtin"
  :global-minor-mode cua-mode
  :init
  (setq cua-enable-cua-keys nil))

(defun my-beginning-of-indented-line (current-point)
  "Change C-a behavior
move line head w/o indent, w/ indent
http://d.hatena.ne.jp/gifnksm/20100131/1264956220"
  (interactive "d")
  (if (s-match "^[ \t]+$"
               (save-excursion
                 (buffer-substring-no-properties
                  (progn (beginning-of-line) (point))
                  current-point)))
      (beginning-of-line)
    (back-to-indentation)))

(leaf *global-key-config
  :bind
  (("\C-a" . my-beginning-of-indented-line)
   ("C-x C-c" . nil)
   ("C-x C-z" . nil)
   ("C-h" . delete-backward-char))
  :init
  ;; C-x C-c -> "exit" command
  (defalias 'exit 'save-buffers-kill-emacs)
  ;; NOTE: This settings is disabled - it collides some org-mode key-bindigs
  ;; windmove
  ;; Shift + Arrow keys
  ;; http://d.hatena.ne.jp/tomoya/20120512/1336832436
  ;; (windmove-default-keybindings)
  )

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
  :global-minor-mode ido-vertical-mode
  :config
  (setq ido-max-window-height 0.75
        do-enable-flex-matching t
        ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(leaf alert
  :ensure t
  :config
  (setq alert-default-style 'libnotify))

(leaf wgrep
  :doc "Edit grep result directry"
  :ensure t)

(leaf wdired
  :doc "Edit dired result directory"
  :ensure t
  :bind
  ((:dired-mode-map
    ("C-c C-e" . wdired-change-to-wdired-mode))))


(leaf *completion
  :config
  (leaf corfu
    :ensure t
    :init
    (global-corfu-mode))

  (leaf cape
    :ensure t
    :bind
    (("<C-tab>" . completion-at-point))
    :custom
    ;; Requires the hunspell-en_us package
    (cape-dict-file . "/usr/share/myspell/dicts/en_US-large.dic")
    :init
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev)
    (add-to-list 'completion-at-point-functions #'cape-dict)
    (add-to-list 'completion-at-point-functions #'cape-symbol)))

(leaf undo-tree
  :ensure t
  :diminish (undo-tree-mode . "🅤")
  :global-minor-mode global-undo-tree-mode
  :config
  (setq undo-tree-auto-save-history nil))

(leaf migemo
  :req "migemo"
  :ensure t
  :commands migemo
  :init
  (setq migemo-command (if (executable-find "cmigemo") "cmigemo" "/usr/local/bin/cmigemo")
        migemo-options '("-q" "--emacs")
        migemo-dictionary (--find
                           (f-exists? it)
                           '("/usr/share/migemo/utf-8/migemo-dict"
                             "/usr/share/cmigemo/utf-8/migemo-dict"
                             "/usr/local/share/migemo/utf-8/migemo-dict"))
        migemo-user-dictionary nil
        migemo-regex-dictionary nil
        migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))

(leaf flycheck
  :diminish (flycheck-mode . "⚠")
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq spaceline-flycheck-bullet "⚠%s")

  (leaf flycheck-posframe
    :ensure t
    :after (flycheck)
    :config
    (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)))

(leaf open-junk-file
  :ensure t
  :commands open-junk-file
  :config
  (setq open-junk-file-format "/var/tmp/emacsjunk/%Y-%m-%d-%H%M%S.")
  ;; open junk file in a current window
  (setq open-junk-file-find-file-function 'find-file))

(leaf *git
  :config
  (leaf magit
    :req "git"
    :ensure t
    :commands magit
    ;; same as IntelliJ IDEA short cut
    :bind (("M-9" . magit-status))
    :config
    (setq magit-diff-refine-hunk t)

    (leaf forge
      :ensure t))

  (leaf git-modes
    :ensure t)

  (leaf git-timemachine
    :ensure t))

(leaf olivetti
  :doc "Let's focus on writing!"
  :ensure t
  :config
  (defun focus-with-olivetti (width)
    "Focus on the active window"
    (interactive "sSet width: ")
    (progn
      (delete-other-windows)
      (olivetti-mode t)
      (olivetti-set-width (string-to-number width)))))

(leaf treemacs
  :ensure t
  :bind
  ;; same as IntelliJ IDEA short cut
  (("M-1" . treemacs))
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
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (leaf treemacs-projectile
    :ensure t
    :after treemacs projectile)

  (leaf treemacs-icons-dired
    :ensure t
    :after treemacs dired
    :config (treemacs-icons-dired-mode))

  (leaf treemacs-magit
    :ensure t
    :after treemacs magit))

(leaf mail-mode
  :mode
  ;; Mutt integration
  (("/tmp/mutt.*" . mail-mode)))

(leaf multiple-cursors
  :ensure t
  :bind (([C-M-return] . mc/edit-lines)))

(leaf expand-region
  :doc "multiple-cursors enhancer"
  :ensure t
  :commands expand-region
  :bind* (("C-,"   . er/expand-region)
          ("C-M-," . er/contract-region)))

(leaf smartrep
  :doc "multiple-cursors enhancer"
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
  :global-minor-mode global-anzu-mode
  :config
  (setq anzu-cons-mode-line-p nil))


(defun my-ace-isearch-consult-line-from-isearch ()
  "Invoke `consult-line' from ace-isearch."
  (interactive)
  (let (($query (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string))))
    (isearch-update-ring isearch-string isearch-regexp)
    (let (search-nonincremental-instead)
      (ignore-errors (isearch-done t t)))
    (consult-line $query)))

(leaf ace-isearch
  :ensure t
  :diminish ace-isearch-mode
  :global-minor-mode global-ace-isearch-mode
  :init
  ;; ace-isearch requires either helm-swoop, helm-occur or swiper.
  ;; so this is a dummy swiper to prevent an error.
  (provide 'swiper)
  :config
  (setq ace-isearch-function 'avy-goto-char
        ace-isearch-function-from-isearch 'my-ace-isearch-consult-line-from-isearch))

(leaf ace-window
  :ensure t
  :bind
  ("C-x o" . other-window-or-split)
  :init
  (defun other-window-or-split ()
    "For ace-window
See http://d.hatena.ne.jp/rubikitch/20100210/emacs"
    (interactive)
    (let ((win-count (length
                      (mapcar #'window-buffer (window-list)))))
      (cond
       ((= win-count 1) (progn
                          (if (< (* 2 (window-height)) (window-width))
                              (split-window-horizontally)
                            (split-window-vertically))
                          (other-window 1)))
       ;; Work around
       ;; Actually, `other-window' is suitable function for this condition,
       ;; but sometimes doesn't work.
       ;; So I use raw `select-window' function instead.
       ((= win-count 2) (select-window
                         (next-window (selected-window) nil nil)))
       ((= win-count 3) (select-window
                         (next-window (selected-window) nil nil)))
       (t (ace-window 1)))))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :custom-face
  (aw-leading-char-face . '((t (:height 4.0 :foreground "#f1fa8c")))))

(leaf visual-regexp-steroids
  :ensure t)

(leaf wrap-region
  :ensure t
  :global-minor-mode wrap-region-mode)

(leaf direnv
  :ensure t
  :config
  (direnv-mode))

(leaf eldoc
  :ensure t
  :diminish (eldoc-mode . "📖"))

(leaf which-key
  :ensure t
  :config
  (which-key-mode))

(leaf *spelling
  :init
  ;; The following lines are hunspell settings with Japanese.
  ;; See: https://www.emacswiki.org/emacs/FlySpell#toc14
  ;; Because an aspell setting
  ;;   (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))
  ;; does not work!
  (defun my-flyspell-ignore-non-ascii (beg end info)
    "Tell flyspell to ignore non ascii characters.
Call this on `flyspell-incorrect-hook'."
    (string-match "[^!-~]" (buffer-substring beg end)))

  :config
  (leaf ispell
    :req "hunspell" "hunspell-en_US"
    :ensure t
    :config
    (setq ispell-program-name (executable-find "hunspell")
          ispell-dictionary "en_US-large"
          ispell-really-hunspell t
          ;; for completion
          ispell-complete-word-dict "/usr/share/hunspell/en_US.dic"))

  (leaf flyspell
    :ensure t
    :hook
    (flyspell-incorrect-hook . my-flyspell-ignore-non-ascii)
    :init
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    :config
    ;; ignore errors on macOS
    (setq flyspell-issue-message-flag nil)
    ;; They conflict with expand-region's bindings
    (define-key flyspell-mode-map (kbd "C-,") nil)
    (define-key flyspell-mode-map (kbd "C-.") nil)))

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

(leaf deadgrep
  :ensure t
  :bind
  (("C-S-f" . deadgrep)))

(leaf dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-banner-logo-title "(start Emacs)"
        dashboard-startup-banner 'logo
        dashboard-items '((agenda . 5)
                          (recents  . 5)
                          (projects . 5)
                          (registers . 5))))

(leaf *visual
  :init
  ;; This line is required to display Kanji font appropriately when the system locale is not Japanese
  (set-language-environment "Japanese")
  (defcustom my-font
    (let* ((fonts (font-family-list))
           (available (-find
                       (lambda (f) (when (-contains? fonts f) f))
                       '("HackGen"
                         "ricty discord nerd font"
                         "Ricty Discord Nerd Font"
                         "ricty discord"
                         "Ricty Discord"
                         "ricty nerd font"
                         "Ricty Nerd Font"
                         "ricty"
                         "Ricty"
                         "cica"
                         "CICA")))
           (size "12.5"))
      (format "%s-%s" available size))
    "Font. It's detected automaticaly by default.")

  :config
  (setq default-frame-alist (list (cons 'font  my-font)))
  (set-frame-font my-font)
  (set-face-font 'default my-font)
  (set-face-font 'variable-pitch my-font)
  ;; Mainly for org-mode inline code and tables
  (set-face-font 'fixed-pitch my-font)
  (set-face-background 'fixed-pitch "#202020")

  (setq
   ;; skip startup screen
   inhibit-startup-screen t
   ;; erase scrach buffer message
   initial-scratch-message "")

  (global-prettify-symbols-mode +1)

  ;; set color, window size
  (when window-system
    (set-frame-parameter nil 'alpha 95))

  (leaf all-the-icons
    :doc "It requires to invoke the following command to install fonts
  M-x all-the-icons-install-fonts"
    :ensure t)

  (leaf atom-one-dark-theme
    :doc "Basic theme settings"
    :ensure t
    :init
    (load-theme 'atom-one-dark t))

  (leaf show-paren-mode
    :doc "High light paren"
    :tag "builtin"
    :init
    (show-paren-mode 1)
    ;; high light inner text of paren when over window
    (setq show-paren-style 'mixed))

  (leaf global-hl-line-mode
    :doc "High light current line"
    :tag "builtin"
    :init
    (setq hl-line-face 'underline)
    (global-hl-line-mode))


  (leaf global-display-line-numbers-mode
    :doc "Line number"
    :init
    (global-display-line-numbers-mode 1))

  (leaf whitespace
    :doc "Show spaces"
    :ensure t
    :diminish (global-whitespace-mode . "🅦")
    :commands whitespace
    :init
    (setq whitespace-style '(face
                             tabs
                             tab-mark
                             spaces
                             lines-tail
                             trailing
                             space-before-tab
                             space-after-tab::space)
          whitespace-line-column 250
          ;; zenkaku space
          whitespace-space-regexp "\\(\x3000+\\)")
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

  (leaf rainbow-delimiters
    :ensure t)

  (leaf highlight-indentation
    :ensure t
    :diminish highlight-indentation-mode)

  (leaf doom-modeline
    :ensure t
    :init
    (doom-modeline-mode 1))

  (leaf volatile-highlights
    :ensure t
    :diminish volatile-highlights-mode
    :config
    (volatile-highlights-mode t))

  (leaf hide-mode-line
    :ensure t
    :hook
    ((treemacs-mode) . hide-mode-line-mode))

  (leaf beacon
    :ensure t
    :config
    (beacon-mode 1))

  (leaf git-gutter-fringe
    ;; FIXME it makes display wrong when `global-linum-mode' is enabled
    :disabled t
    :doc "Show an icon indicating whether a line has been changed from last commit"
    :ensure t
    :diminish git-gutter-mode
    :init
    (setq git-gutter-fr:side 'right-fringe)
    :config
    (global-git-gutter-mode)))

(leaf ddskk
  :ensure t
  :bind
  (("C-o" . skk-mode))
  :init
  (defcustom my-skk-jisyo-root
    (-find 'f-directory? '("/usr/share/skk" "~/skk"))
    "SKK dictionary path. Override it for each platform")
  (setq
   ;; enable AZIK
   skk-use-azik t
   ;; disable skk-isearch for migemo
   skk-isearch-mode-enable nil
   skk-search-start-mode 'latin
   ;; candidates position
   skk-show-tooltip t
  ;; dynamic completion
  ;;   てん -> てんさい, てんしょく、てんかん
   skk-dcomp-activate t
   skk-dcomp-multiple-activate t
   skk-dcomp-multiple-rows 10
   ;; dictionary
   skk-large-jisyo (concat my-skk-jisyo-root "/SKK-JISYO.L")
   skk-extra-jisyo-file-list (--map (concat my-skk-jisyo-root it)
                                    '("/SKK-JISYO.geo"
                                      "/SKK-JISYO.jinmei"
                                      "/SKK-JISYO.propernoun"
                                      "/SKK-JISYO.station")))

  (leaf ddskk-posframe
    :ensure t
    :diminish
    :config
    (ddskk-posframe-mode t)))

(leaf dumb-jump
  :ensure t
  :config
  ;; See the `dumb-jump-xref-activate' function document. It doesn't override
  ;; the behavior, but appends a fallback method with dumb-jump.
  ;; "It is recommended to add it to the end, so that it only gets activated
  ;; when no better"
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  ;; for completion frameworks
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

(leaf paredit
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'common-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode))

(leaf projectile
  :ensure t
  :commands projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " 📁 %s" (projectile-project-name))))

  (leaf consult-projectile
    :ensure t))

(leaf yasnippet
  :ensure t
  :diminish (yas-minor-mode . "🅨")
  :init
  (yas-global-mode 1)
  :config
  (leaf yasnippet-snippets
    :ensure t)
  :bind
  ((:yas-minor-mode-map
    ("M-=" . yas-insert-snippet))))

(leaf restclient
  :ensure t
  :init
  ;; restclient-jq is not provided by MELPA
  ;; therefore use el-get instead
  (el-get-bundle restclient-jq
    :url "https://raw.githubusercontent.com/pashky/restclient.el/master/restclient-jq.el")
  :config
  (require 'restclient-jq))

(leaf jq-mode
  :doc "This package is required by restclient-jq"
  :ensure t)

(leaf slime
  :req "sbcl"
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  :ensure t
  :config
  (slime-setup '(slime-repl slime-fancy slime-banner)))

(leaf *c-c++
  :config
  ;; flycheck
  (add-hook 'c-mode-common-hook 'flycheck-mode)

  ;; compile C-c
  (add-hook 'c-mode-common-hook
            (lambda ()
              (define-key mode-specific-map "c" 'compile)))

  (leaf google-c-style
    :ensure t
    :commands google-c-style
    :init
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    (add-hook 'c-mode-common-hook 'google-make-newline-indent))

  (setq
   ;; GDB
   gdb-many-windows t
   ;; show I/O buffer
   gdb-use-separate-io-buffer t)
  ;; show value of variable when mouse cursor on
  (add-hook 'gdb-mode-hook (lambda () (gud-tooltip-mode t))))

(leaf clojure-mode
  :ensure t
  :init
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

  :config
  ;; prittify symbols
  (my-macro/prettify-symbols
   clojure-mode-hook
   (-concat '(("fn" . ?λ))
            my-const/logical-prettify-symbols-alist
            my-const/relational-prettify-symbols-alist))

  (leaf cider
    :ensure t
    :init
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'cider-mode-hook #'clj-refactor-mode)
    (add-hook 'cider-mode-hook #'eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'eldoc-mode)
    :config
    (setq
     ;; Hide the *nrepl-connection* and *nrepl-server* buffers
    ;; from appearing in some buffer switching commands like 'C-x b'
     nrepl-hide-special-buffers t
     ;; The REPL buffer name  will look like cider project-name:port
     nrepl-buffer-name-show-port t
     ;; Enable CIDER and figwheel interaction
     cider-cljs-lein-repl "(do (require 'figwheel-sidecar.repl-api)
                             (figwheel-sidecar.repl-api/start-figwheel!)
                             (figwheel-sidecar.repl-api/cljs-repl))"))

  (leaf clj-refactor
    :ensure t
    :config
    (defun my-clojure-mode-hook ()
      (clj-refactor-mode 1)
      (yas-minor-mode 1) ; for adding require/use/import statements
      ;; This choice of keybinding leaves cider-macroexpand-1 unbound
      (cljr-add-keybindings-with-prefix "C-c C-m"))
    (add-hook 'clojure-mode-hook #'my-clojure-mode-hook))

  ;; compojure indentation
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
    (add-to-list 'compilation-error-regexp-alist-alist
                 '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0)))

  ;; rainbow delimiters
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'highlight-indentation-mode))

(leaf css
  :after flycheck
  :init
  (add-to-list 'auto-mode-alist '("\\.css" . css-mode))
  (add-hook 'css-mode-hook 'flycheck-mode)
  (setq css-indent-offset 2))

(leaf dockerfile-mode
  :ensure t
  :mode (("Dockerfile" . dockerfile-mode)))

(leaf elm-mode
  :ensure t
  :doc "Install :req packages with `npm i -g`"
  :req "elm" "elm-test" "elm-format" "@elm-tooling/elm-language-server"
  :config
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))

(leaf emacs-lisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-indentation-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(defconst lisp--prettify-symbols-alist
  (-concat my-const/lambda-prettify-symbols-alist
           my-const/logical-prettify-symbols-alist
           my-const/relational-prettify-symbols-alist))

(leaf nginx-mode
  :ensure t)

(leaf fish-mode
  :req "fish"
  :ensure t
  :custom
  (fish-indent-offset . 2))

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

(leaf js-mode
  :doc "Only for a JSON file"
  :config
  (setq js-indent-level 2))

(leaf js2-mode
  :ensure t
  :req "eslint"
  :after flycheck
  :commands js2-mode
  :mode (("\\.js$" . js2-mode))
  :init
  ;; flycheck
  ;;  npm: eslint
  (add-hook 'js2-mode-hook 'flycheck-mode)
  ;; disable jshint for eslint
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (global-set-key [f5] 'slime-js-reload)
  (add-hook 'js2-mode-hook
            (lambda ()
              (slime-js-minor-mode 1)))
  ;; prettify symbols
  (my-macro/prettify-symbols
   js2-mode-hook
   (-concat '(("function" . ?ƒ))
            my-const/arrow-prettify-symbols-alist
            my-const/relational-prettify-symbols-alist))
  :config
  (setq js2-basic-offset 2
        js-indent-level 2))

(leaf haskell-mode
  :ensure t
  :mode ((".xmobarrc" . haskell-mode))
  :init
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode))

(leaf kotlin-mode
  :ensure t)

(leaf lua-mode
  :ensure t
  :custom
  (lua-indent-level . 2))

(leaf markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (put 'dired-find-alternate-file 'disabled nil))

(leaf plantuml-mode
  :ensure t
  :mode
  (("\\.plantuml\\'" . plantuml-mode))
  :config
  (setq plantuml-jar-path org-plantuml-jar-path)
  (setq plantuml-default-exec-mode 'jar)

  (leaf flycheck-plantuml
    :ensure t
    :config
    (flycheck-plantuml-setup)))

(leaf php-mode
  :ensure t
  :commands php-mode
  :mode (("\\.php$" . php-mode))
  :init
  (setq php-mode-force-pear t)
  (add-hook 'php-mode-hook
            (lambda ()
              (flycheck-mode t)
              (setq tab-width 4)
              (setq c-basic-offset 4))))

(leaf python-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (add-hook 'python-mode-hook
            (lambda()
              (setq indent-tabs-mode nil
                    indent-level 4
                    python-indent 4
                    tab-width 4)))
  ;; flycheck
  ;;  required package: pylint
  (add-hook 'python-mode-hook 'flycheck-mode)
  ;; highlight indentation
  (add-hook 'python-mode-hook 'highlight-indentation-mode))

(leaf lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; Use Corfu instead of Company
  ;; See https://github.com/minad/corfu/wiki#configuring-corfu-for-lsp-mode
  (lsp-completion-provider . :none)
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(flex))) ; Configure flex for Corfu
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion))

(leaf rustic
  :doc "Rust"
  :ensure t
  :commands
  (rustic-mode)
  :mode
  (("\\.rs\\'" . rustic-mode))
  :config
  (setq rustic-lsp-server 'rust-analyzer)
  (push 'rustic-clippy flycheck-checkers)
  (setq rustic-ansi-faces
        ["black"
         "#FF3F43"                ; changed from "red3". it's too dark
         "green3"
         "yellow3"
         "#4068FF"               ; changed from "blue2". it's too dark
         "magenta3"
         "cyan3"
         "white"]))

(leaf ruby-mode
  :mode (("Gemfile" . ruby-mode)
         ("Rakefile" . ruby-mode)
         ("Vagrantfile" . ruby-mode)
         ("\\.rake" . ruby-mode))
  :interpreter "ruby"
  :init
  ;; highlight indentation
  (add-hook 'ruby-mode-hook 'highlight-indentation-mode)
  :config
  ;; flycheck
  (add-hook 'ruby-mode-hook 'flycheck-mode)
  ;; smartparens
  ;; highlight block
  (add-hook 'ruby-mode-hook 'show-smartparens-mode)
  ;; disable magic comment
  (defun ruby-mode-set-encoding () nil)
  ;; prittify symbols
  (my-macro/prettify-symbols
   ruby-mode-hook
   (-concat my-const/lambda-prettify-symbols-alist
            my-const/arrow-prettify-symbols-alist
            my-const/relational-prettify-symbols-alist))

  ;; ruby-electric
  (leaf ruby-electric
    :diminish ruby-electric-mode
    :config
    (add-hook 'ruby-mode-hook
              (lambda()
                (ruby-electric-mode t)))
    (defun ruby-insert-end ()
      "bugfix for ruby-electric-space
'Symbol's function definition is void: ruby-insert-end'"
      (interactive)
      (insert "end")
      (ruby-indent-line t)
      (end-of-line)))

  ;; inf-ruby
  (leaf inf-ruby
    :commands inf-ruby
    :init
    (setq inf-ruby-default-implementation "pry"
          inf-ruby-eval-binding "Pry.toplevel_binding")
    (add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)))

(leaf scala-mode
  :ensure t)

(leaf scheme-mode
  :commands scheme-mode
  :init
  (defun scheme-other-window ()
    "Run scheme on other window"
    (interactive)
    (switch-to-buffer-other-window(get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name))
 
  (setq process-coding-system-alist (cons '("gosh" utf-8 . utf-8) process-coding-system-alist)
        scheme-program-name "gosh -i")

  :config
  (add-hook 'scheme-mode-hook 'highlight-indentation-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)
  (my-macro/prettify-symbols
   scheme-mode-hook
   (-concat my-const/lambda-prettify-symbols-alist
            my-const/logical-prettify-symbols-alist
            my-const/relational-prettify-symbols-alist))

  (leaf run-scheme
    :commands run-scheme))

(leaf shell-script-mode
  :mode (("PKGBUILD" . shell-script-mode)
         ("\\.install$" . shell-script-mode))
  :init
  (setq sh-basic-offset 2
        sh-indentation 2))

(leaf terraform-mode
  :ensure t)

(leaf typescript
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  (leaf tide
    :ensure t
    :mode (("\\.ts" . tide-mode)
           ("\\.tsx" . tide-mode))
    :config
    ;; formats the buffer before saving
    (add-hook 'before-save-hook 'tide-format-before-save)
    (add-hook 'typescript-mode-hook #'setup-tide-mode)))


(leaf yaml-mode
  :commands yaml-mode
  :ensure t
  :mode (("\\.ya?ml$" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  ;; flycheck
  (add-hook 'yaml-mode-hook 'flycheck-mode))

(leaf web-mode
  :ensure t
  :req "eslint"
  :commands web-mode
  :after flycheck
  :init
  (add-hook 'web-mode-hook 'flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; disable jshint for eslint
  (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 2
          web-mode-css-offset           2
          web-mode-css-indent-offset    2
          web-mode-code-indent-offset   2
          web-mode-html-offset          2
          web-mode-script-offset        2
          web-mode-php-offset           4))
  (add-hook 'web-mode-hook  'my-web-mode-hook)
  ;; prettify symbols
  (my-macro/prettify-symbols
     web-mode-hook
     (-concat '(("function" . ?ƒ))
              my-const/arrow-prettify-symbols-alist
              my-const/relational-prettify-symbols-alist))
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx$\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))


(leaf org-mode
  :mode (("\\.org$" . org-mode))
  ;; FIXME it doesn't work
  :bind ((:org-mode-map
          :package org-mode
          ("C-," . nil)))
  :init
  (setq org-agenda-files (list "~/Documents/org"))
  ;; syntax highlight within begin_src block
  (setq org-src-fontify-natively t)

  (leaf org-babel
    :doc "How to use org-babel
The following is an example for PlantUML

  #+BEGIN_SRC plantuml :file example.png :cmdline -charset UTF-8
  animal <|-- sheep
  #+END_SRC

Then, type `'C-c C-c` inside BEGIN_SRC ~ END_SRC
It creates an image file.
To show the image file inline, use the following.
  org-toggle-inline-images (C-c C-x C-v)"
    :init
    (setq org-plantuml-jar-path
      (--find
       (f-exists? it)
       '("/usr/share/java/plantuml/plantuml.jar")))
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((plantuml . t)
       (shell . t)
       (python . t))))

  (leaf org-pomodoro
    :ensure t
    :custom
    `((org-pomodoro-long-break-length . 15)
      (org-pomodoro-audio-player . ,(or (executable-find "pacat")
                                        (executable-find "paplay")
                                        (executable-find "aplay")
                                        (executable-find "afplay")))
      (org-pomodoro-format . "🍅%s")
      (org-pomodoro-short-break-format . "☕%s")
      (org-pomodoro-long-break-format . "🌴%s"))
    :init
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

    (leaf sound-wav
      :doc "Required by org-pomodoro implicitly"
      :ensure t))

  (leaf org-analyzer
    :doc "Visualize org-mode time spend"
    :ensure t)

  (leaf org-alert
    :doc "Set alerts for scheduled tasks"
    :after alert
    :ensure t)

  (leaf org-superstar
    :doc "Beautify org-mode appearance"
    :ensure t
    :hook
    (org-mode-hook . (lambda () (org-superstar-mode 1)))
    :config
    ;; Show a "DONE" annotated task with a checkbox
    (setq org-superstar-special-todo-items t)))

(leaf org-roam
  :ensure t
  :init
  (let ((roam-path (format "%s/Documents/org-roam" (getenv "HOME"))))
    (unless (f-exists? roam-path)
      (make-directory roam-path))
    (setq org-roam-directory   roam-path)
    (setq org-roam-db-location (format "%s/org-roam.db" roam-path)))
  :config
  (org-roam-db-autosync-mode)
  (defun gen-subtemplates (roam-path)
    "Dynamically generates Org-roam capture templates with checking subdirectories. This function assumes there's no directory named as `default', and all subdirectories have different initials."
    (->> (directory-files roam-path t "^[^.]")
         (-filter #'file-directory-p)
         (--map (s-chop-prefix (concat roam-path "/") it))
         (--map `(,(s-left 1 it)
                  ,it
                  plain
                  "%?"
                  :target (file+head ,(concat it "/%<%Y%m%d%H%M%S>.org")
                                     "#+title: ${title}\n#+date: %U\n")
                  :unnarrowed t))))
  (setq org-roam-capture-templates
        (-concat '(("0" "default" plain "%?"
                    :target (file+head "%<%Y%m%d%H%M%S>.org"
                                       "#+title: ${title}\n#+date: %U\n")
                    :unnarrowed t))
                 (gen-subtemplates org-roam-directory)))
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: Daily %<%Y-%m-%d>\n")))))

(leaf *ai
  :config
  (defun my-speech-note-import ()
    "Run the speech note processing script and insert the result into the current buffer.
The script is executed with the -r option to remove the original files after processing."
    (interactive)
    (let* ((script-path (expand-file-name "~/bin/speech_note.sh"))
           (input-dir (expand-file-name "~/Subsync/iPhone/speech_note"))
           (model "gemma2:9b")
           (command (format "%s -r -m %s %s 2>/dev/null"
                            (shell-quote-argument script-path)
                            (shell-quote-argument model)
                            (shell-quote-argument input-dir)))
           (output (shell-command-to-string command)))
      (save-excursion
        (goto-char (point-max))
        (insert output))
      (message "Inserted processed speech notes into the current buffer.")))

  (leaf ellama
    :ensure t
    :bind ("C-c e" . ellama-transient-main-menu)
    ;; send last message in chat buffer with C-c C-c
    :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
    :init
    ;; setup key bindings
    ;; (setopt ellama-keymap-prefix "C-c e")
    ;; language you want ellama to translate to
    (setopt ellama-language "Japanese")
    ;; could be llm-openai for example
    (require 'llm-ollama)
    (setopt ellama-provider
            (make-llm-ollama
             ;; this model should be pulled to use it
             ;; value should be the same as you print in terminal during pull
             :chat-model "gemma2:9b"
             :embedding-model "nomic-embed-text"
             :default-chat-non-standard-params '(("num_ctx" . 8192))))
    (setopt ellama-summarization-provider
            (make-llm-ollama
             :chat-model "gemma2:9b"
             :embedding-model "nomic-embed-text"
             :default-chat-non-standard-params '(("num_ctx" . 32768))))
    (setopt ellama-coding-provider
            (make-llm-ollama
             :chat-model "qwen2.5-coder:7b"
             :embedding-model "nomic-embed-text"
             :default-chat-non-standard-params '(("num_ctx" . 32768))))
    ;; Predefined llm providers for interactive switching.
    ;; You shouldn't add ollama providers here - it can be selected interactively
    ;; without it. It is just example.
    (setopt ellama-providers
            '(("gemma2" . (make-llm-ollama
                           :chat-model "gemma2:9b"
                           :embedding-model "gemma2:9b"))
              ("elyza" . (make-llm-ollama
                          :chat-model "hf.co/elyza/Llama-3-ELYZA-JP-8B-GGUF"
                          :embedding-model "hf.co/elyza/Llama-3-ELYZA-JP-8B-GGUF"))
              ("elyza_e" . (make-llm-ollama
                            :chat-model "hf.co/elyza/Llama-3-ELYZA-JP-8B-GGUF"
                            :embedding-model "nomic-embed-text"))
              ("qwen2.5" . (make-llm-ollama
                            :chat-model "qwen2.5:7b"
                            :embedding-model "qwen2.5:7b"))
              ("qwen2.5_e" . (make-llm-ollama
                              :chat-model "qwen2.5:7b"
                              :embedding-model "nomic-embed-text"))))
    ;; Naming new sessions with llm
    (setopt ellama-naming-provider
            (make-llm-ollama
             :chat-model "hf.co/alfredplpl/gemma-2-2b-jpn-it-gguf"
             :embedding-model "nomic-embed-text"
             :default-chat-non-standard-params '(("stop" . ("\n")))))
    (setopt ellama-naming-scheme 'ellama-generate-name-by-llm)
    ;; Translation llm provider
    (setopt ellama-translation-provider
            (make-llm-ollama
             :chat-model "hf.co/alfredplpl/gemma-2-2b-jpn-it-gguf"
             :embedding-model "nomic-embed-text"
             :default-chat-non-standard-params
             '(("num_ctx" . 32768))))
    (setopt ellama-extraction-provider (make-llm-ollama
                                        :chat-model "qwen2.5-coder:7b"
                                        :embedding-model "nomic-embed-text"
                                        :default-chat-non-standard-params
                                        '(("num_ctx" . 32768))))
    ;; customize display buffer behavior
    ;; see ~(info "(elisp) Buffer Display Action Functions")~
    (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
    (setopt ellama-instant-display-action-function #'display-buffer-at-bottom)
    :config
    ;; show ellama context in header line in all buffers
    (ellama-context-header-line-global-mode +1))

  (leaf chatgpt-shell
    :ensure t
    :config
    (setq chatgpt-shell-openai-key
          (lambda ()
            (nth 0 (process-lines "pass" "show" "openai-key"))))))

(leaf *information
  :config
  (leaf elpher
    :ensure t)

  (defconst my/elfeed-setting-dir "~/Dropbox/Settings")

  (leaf elfeed
    :ensure t
    :init
    :config
    (setq elfeed-db-directory (f-join my/elfeed-setting-dir "elfeeddb"))
    (setq-default elfeed-search-filter "@6-months-ago +unread -sub"))

  (leaf elfeed-org
    :ensure t
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files (list (f-join my/elfeed-setting-dir "elfeed.org")))))

(leaf *char-encoding
  :config
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-buffer-file-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8)
  (prefer-coding-system 'utf-8-unix))

(leaf *mac
  :when (eq system-type 'darwin)
  ;; Need to place hunspell dictionaries
  ;; to the path which is included in the results of `hunspell -D`
  ;;
  ;; $ cd ~/Library/Spelling
  ;; $ wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.aff
  ;; $ wget https://cgit.freedesktop.org/libreoffice/dictionaries/plain/en/en_US.dic
  (setq ispell-dictionary "en_US")
  (add-to-list 'ispell-dictionary-alist
               '("en_US" "[[:alpha:]]" "[^[:alpha:]]" "'" t ("-d" "en_US") nil utf-8))
  ;; https://stackoverflow.com/questions/3961119/working-setup-for-hunspell-in-emacs
  (defun ispell-get-coding-system () 'utf-8)
  ;; beautify powerline
  ;; https://github.com/milkypostman/powerline/issues/54
  (setq ns-use-srgb-colorspace nil
        alert-default-style 'osx-notifier))


;;; --- experimental ---

(require 'consult)

(defgroup brain-search nil
  "Search brain knowledge base with Consult."
  :group 'convenience
  :prefix "brain-search-")

(defcustom brain-search-command "brain"
  "Path to the brain command."
  :type 'string
  :group 'brain-search)

(defcustom brain-search-jq-command "jq"
  "Path to the jq command."
  :type 'string
  :group 'brain-search)

;;;###autoload
(defun brain-search ()
  "Search brain knowledge base and select a file with Consult.
Interactively gets a query, runs brain search, and presents results with Consult."
  (interactive)
  (let* ((query (read-string "Brain search query: "))
         (shell-command (format "%s --mode search-only --format json %s | %s -r '.matched_files.[].path'"
                                brain-search-command
                                (shell-quote-argument query)
                                brain-search-jq-command))
         (output (shell-command-to-string shell-command))
         (paths (split-string output "\n" t)))
    (if (null paths)
        (message "No matching files found for query: %s" query)
      (let ((selected (consult--read paths
                                     :prompt "Select file: "
                                     :category 'file
                                     :sort nil
                                     :require-match t
                                     :preview-key consult-preview-key
                                     :state (consult--file-preview)
                                     :history 'brain-search-history)))
        (when selected
          (find-file selected))))))

;;; init.el ends here
