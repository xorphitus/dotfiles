;;; PHP
(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php" . php-mode))
(setq php-mode-force-pear t)

(add-hook 'php-mode-hook 'flycheck-mode)

;;; MMM
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(set-face-background 'mmm-default-submode-face nil)
;; for php-mode in html-helper-mode
(mmm-add-mode-ext-class nil "\\.php?\\'" 'html-php)
(mmm-add-classes
 '((html-php
    :submode php-mode
    :front "<\\?\\(php\\)?"
    :back "\\?>")))
(add-to-list 'auto-mode-alist '("\\.php?\\'" . html-mode))
