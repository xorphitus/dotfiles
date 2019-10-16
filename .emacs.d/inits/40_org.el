;;; org.el --- constants

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has org-mode settings.

;;; Code:

(setq org-agenda-files (list "~/Documents/org"))

(set-face-attribute 'org-level-1 nil :height 1.4)

;; It conflicts with expand-region's bindings
(define-key org-mode-map (kbd "C-,") nil)

;; beautify org-mode list bullets
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :custom
  (org-bullets-bullet-list '("🌕" "🌔" "🌓" "🌒" "🌑")))

;; pomodoro

;; need to override this function
;; since https://github.com/syohex/emacs-sound-wav which is depended by org-pomodoro
;; does not support PulseAudio's pacat/paplay
(defun sound-wav--do-play (files)
  (cond ((executable-find "afplay")
         ;; for macOS
         (sound-wav--do-play-by-afplay files))
        ((executable-find "pacat")
         ;; for PulseAudio
         (deferred:$
           ;; max volume: 65536 (100%)
           (apply 'deferred:process "pacat" "--volume=49152" files)))
        (t
         (error "Not found wav player on your system!!"))))

(use-package org-pomodoro
  :after
  org-agenda
  :custom
  (org-pomodoro-audio-player (or (executable-find "pacat")
                                 (executable-find "paplay")
                                 (executable-find "aplay")
                                 (executable-find "afplay")))
  (org-pomodoro-format "🍅%s")
  (org-pomodoro-short-break-format "☕%s")
  (org-pomodoro-long-break-format  "🌴%s"))

;; set alerts for scheduled tasks
(use-package org-alert
  :config
  (add-hook 'org-mode-hook (lambda () (org-alert-enable))))
