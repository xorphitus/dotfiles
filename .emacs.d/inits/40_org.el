;;; org.el --- constants

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has org-mode settings.

;;; Code:

(setq org-agenda-files (list "~/Documents/org"))

;; (set-face-attribute 'org-level-1 nil :height 1.4)

;; It conflicts with expand-region's bindings
(define-key org-mode-map (kbd "C-,") nil)

;; beautify org-mode list bullets
(use-package org-bullets
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

;; pomodoro

;; need to override this function
;; since https://github.com/syohex/emacs-sound-wav which is depended by org-pomodoro
;; does not support PulseAudio's pacat/paplay
(defun sound-wav--do-play (files)
  (let* ((vol-percent 75)
         (vol (round (/ (* 65536 vol-percent) 100))))
    (if (executable-find "afplay")
        ;; for macOS
        (sound-wav--do-play-by-afplay files)
      ;; for PulseAudio
      (deferred:$
        (apply 'deferred:process org-pomodoro-audio-player (concat "--volume=" (number-to-string vol)) files)))))

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
