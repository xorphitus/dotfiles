(require 'yasnippet)
(yas-global-mode 1)

;; snippets path
;;(setq yas-snippet-dirs
;;      '("~/.emacs.d/my_snippets"
;;        "~/.emacs.d/vendor_snippets"))

;; key bindings
(define-key yas-minor-mode-map (kbd "M-=") 'yas-insert-snippet)

;; helm interface
(defun my-yas/prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
                      collect (or (and display-fn (funcall display-fn choice))
                                  coice)))
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
