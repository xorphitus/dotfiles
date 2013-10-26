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

;; mermelada - elisp repository
;;  http://marmalade-repo.org/
;;  'package-list-packages
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

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
