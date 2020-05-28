(defconst my/elfeed-setting-dir "/path/to")

(use-package elfeed
  :config
  (setq elfeed-db-directory (f-join my/elfeed-setting-dir "elfeeddb"))
  (setq-default elfeed-search-filter "@6-months-ago +unread -sub"))

(use-package elfeed-org
  :after (elfeed)
  :config
  (elfeed-org)
  (setq rmh-elfeed-org-files (list (f-join my/elfeed-setting-dir "elfeed.org"))))
