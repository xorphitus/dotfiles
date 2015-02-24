;;; yasnippet.el --- yasnippet settings

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has yasnippet settings.

;;; Code:

(use-package yasnippet
  :config
  (progn
    (yas-global-mode 1)

    (defun my-yas/prompt (prompt choices &optional display-fn)
      (let* ((names (--map (or (and display-fn (funcall display-fn it))
                               it)
                           choices))
             (selected (helm-other-buffer
                        `(((name . ,(format "%s" prompt))
                           (candidates . names)
                           (action . (("Insert snippet" . (lambda (arg) arg))))))
                        "*helm yas/prompt*")))
        (if selected
            (let ((n (position selected names :test 'equal)))
              (nth n choices))
          (signal 'quit "user quit!"))))
    (custom-set-variables '(yas/prompt-functions '(my-yas/prompt)))

    (define-key yas-minor-mode-map (kbd "M-=") 'yas-insert-snippet)))
