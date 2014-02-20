;; edit grep result directry
;;  https://raw.github.com/mhayashi1120/Emacs-wgrep/master/wgrep.el
(require 'wgrep)

;; edit dired result directory
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;; auto-complete
;;  http://cx4a.org/software/auto-complete/index.ja.html
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; select completion target by using C-n/C-p
(setq ac-use-menu-map t)

;; yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; undo-tree.el
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

;; migemo
(require 'migemo)
(setq migemo-command "cmigemo")
(setq migemo-options '("-q" "--emacs"))
(setq migemo-dictionary
      (find-if
       (lambda (dict) (file-exists-p dict))
         '("/usr/share/migemo/utf-8/migemo-dict"
           "/usr/share/cmigemo/utf-8/migemo-dict"
           "/usr/local/share/migemo/utf-8/migemo-dict")))
(setq migemo-user-dictionary nil)
(setq migemo-regex-dictionary nil)
(setq migemo-coding-system 'utf-8-unix)
(load-library "migemo")
(migemo-init)

;; all-ext
;;  http://d.hatena.ne.jp/rubikitch/20130202/all
(require 'all-ext)

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

;; flycheck
(if window-system
    (setq flycheck-display-errors-function
          '(lambda (errors)
             (-when-let (messages (-keep #'flycheck-error-message errors))
               (popup-tip (s-join "\n\n" messages))))))

;; junk file
(require 'open-junk-file)

;; magit.el
(require 'magit)

;; quickrun.el
;; http://d.hatena.ne.jp/syohex/20111201/1322665378
(require 'quickrun)
(global-set-key [(shift f5)] 'quickrun)

;; shell-pop.el
;;  http://www.emacswiki.org/emacs/download/shell-pop.el
(require 'shell-pop)
(global-set-key [f8] 'shell-pop)

;; projectile
(require 'projectile)
(projectile-global-mode)

;; multiple-cursors and enhancer
(require 'expand-region)
(require 'multiple-cursors)
(require 'smartrep)

(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

(global-set-key (kbd "<C-M-return>") 'mc/edit-lines)
(smartrep-define-key
 global-map "C-." '(("n" . 'mc/mark-next-like-this)
                    ("p" . 'mc/mark-previous-like-this)
                    ("P" . 'mc/unmark-next-like-this)
                    ("N" . 'mc/unmark-previous-like-this)
                    ("*" . 'mc/mark-all-like-this)))

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
