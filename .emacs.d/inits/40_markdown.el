;;; Markdown
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(setq auto-mode-alist
      (cons '("\\.text" . markdown-mode) auto-mode-alist))
(put 'dired-find-alternate-file 'disabled nil)