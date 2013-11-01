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
            actionscript-mode
            clojure-mode
            coffee-mode
            color-theme
            edbi
            emmet-mode
            haml-mode
            ipython
            js2-mode
            ;;js2-refactor
            less-css-mode
            markdown-mode
            nrepl
            paredit
            php-mode
            open-junk-file
            rinari
            ruby-block
            ruby-compilation
            sass-mode
            scala-mode
            scss-mode
            web-mode
            ;;; --- others ---
            auto-complete
            auto-install
            flycheck
            flymake-cursor
            ;;all
            ;;all-ext
            helm
            helm-ag
            init-loader
            ;;multiple-cursors
            magit
            migemo
            popup
            python-mode
            quickrun
            shell-pop
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
