;;; xorphitus elisp

;; enable common lisp
(require 'cl)

;; set load path
(let ((default-directory "~/.emacs.d/elisp/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory "~/.emacs.d/elpa/"))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; elpa
(setq package-archives '(("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun xorphitus-setup ()
  (interactive)
  (progn (package-refresh-contents)
         (mapc
          (lambda (pkg)
            (or (package-installed-p pkg)
                (package-install pkg)))
          '(;;; --- coding ---
            ac-nrepl
            actionscript-mode
            cider
            clojure-cheatsheet
            clojure-mode
            coffee-mode
            color-theme
            edbi
            emmet-mode
            google-c-style
            haml-mode
            ipython
            js2-mode
            ;;js2-refactor
            less-css-mode
            markdown-mode
            php-mode
            rinari
            ruby-electric
            ruby-compilation
            sass-mode
            scala-mode
            scss-mode
            slim-mode
            web-mode
            ;;; --- others ---
            auto-complete
            auto-install
            expand-region
            flycheck
            flymake-cursor
            ;;all
            ;;all-ext
            git-gutter-fringe
            helm
            helm-ag
            helm-projectile
            init-loader
            ;;multiple-cursors
            magit
            migemo
            multiple-cursors
            open-junk-file
            popup
            projectile
            python-mode
            quickrun
            rainbow-delimiters
            shell-pop
            smartparens
            smartrep
            twittering-mode
            undo-tree
            wgrep
            yasnippet))))

;; init loader
(require 'init-loader)
(init-loader-load "~/.emacs.d/inits")

;; detect error file
(defun init-loader-re-load (re dir &optional sort)
  (let ((load-path (cons dir load-path)))
    (dolist (el (init-loader--re-load-files re dir sort))
      (condition-case e
          (let ((time (car (benchmark-run (load (file-name-sans-extension el))))))
            (init-loader-log (format "loaded %s. %s" (locate-library el) time)))
        (error
         (init-loader-error-log (format "%s. %s" (locate-library el) (error-message-string e))))))))

;;; UTF-8
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
