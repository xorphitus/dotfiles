;;; lsp.el --- Language Server Protocol

;; Author: xorphitus <xorphitus@gmail.com>

;;; Commentary:
;;
;; This file has LSP settings.

;;; Code:

(use-package eglot)

;; The following is copied from
;; https://github.com/akash-akya/emacs-flymake-cursor
;; The original flymake-cursor on MELPA is deprecated now
(defgroup flymake-cursor nil
  "Show flymake errors for current line in message area."
   :group 'tools)

(defcustom flymake-cursor-error-display-delay 0.9
  "Delay in seconds to wait before displaying flymake errors for the current line."
  :group 'flymake-cursor
  :type 'number)

(defcustom flymake-cursor-number-of-errors-to-display 1
  "Number of flymake errors to display if there are more than one.
If set to nil, all errors for the line will be displayed.
If there are more errors than can be displayed in the minibuffer, the
first ones will be scrolled off. You will probably want to set this
variable to a value consistent with your `max-mini-window-height'
setting."
  :group 'flymake-cursor
  :type '(choice integer (const nil)))

(defcustom flymake-cursor-auto-enable t
  "Whether flymake-cursor should automatically enable itself whenever
flymake is enabled.
If set to t, flymake-cursor will turn on whenever flymake does.
If set to nil, flymake-cursor will need to be manually enabled.
Regardless of this setting, flymake-cursor will always disable
itself automatically when flymake is disabled, to prevent
errors."
  :group 'flymake-cursor
  :type 'boolean)

(defvar flymake-cursor-errors-at-point nil
  "Errors at point, after last command")

(defvar flymake-cursor-error-display-timer nil
  "A timer; when it fires, it displays the stored error message.")

;;;###autoload
(define-minor-mode flymake-cursor-mode
  "Minor mode to show `flymake-mode' errors for the current line in the
message area.
When called interactively, toggles the minor mode.
With arg, turn Flymake Cursor mode on if and only if arg is positive.
Usually `flymake-cursor-mode' is enabled and disabled automatically with
`flymake-mode' for the current buffer and you will not need to toggle
the mode directly."
  :group 'flymake-cursor
  (cond

    ;; Turning the mode ON.
    (flymake-cursor-mode
      (add-hook 'post-command-hook 'flymake-cursor-show-errors-at-point-pretty-soon nil t))
    ;; Turning the mode OFF.
    (t
      (flymake-cursor-cancel-error-display-timer)
      (remove-hook 'post-command-hook 'flymake-cursor-show-errors-at-point-pretty-soon t))))

(defun flymake-cursor-get-errors-at-point ()
  "Gets the first `flymake-cursor-number-of-errors-to-display` flymake errors on the line at point."
  (let ((line-err-info-list (flymake-cursor-get-errors)))
    (if flymake-cursor-number-of-errors-to-display
        (butlast line-err-info-list (- (length line-err-info-list) flymake-cursor-number-of-errors-to-display))
      line-err-info-list)))

(defun flymake-cursor-get-errors ()
  (cond ((boundp 'flymake-err-info)  ; emacs < 26
         (let ((lineno (line-number-at-pos)))
           (car (flymake-find-err-info flymake-err-info lineno))))
        ((and (fboundp 'flymake-diagnostic-text)
              (fboundp 'flymake-diagnostics))  ; emacs >= 26
         (flymake-diagnostics (point)))))

(defun flymake-cursor-pyflake-determine-message (error)
  "pyflake is flakey if it has compile problems, this adjusts the
message to display, so there is one ;)"
  (cond ((not (or (eq major-mode 'Python) (eq major-mode 'python-mode) t)))
        ((fboundp 'flymake-diagnostic-text)
         (let ((msg (flymake-diagnostic-text error)))
           (if (null msg) msg (format "compile error, problem on line %s" msg))))
        (t
         (flymake-ler-text error))))

(defun flymake-cursor-safe-to-display ()
  "Returns t if Flymake Cursor is safe to display to the minibuffer or nil if
something else is using the message area."
  ;;  Don't trash the minibuffer while they're being asked a question.
  (not (or (active-minibuffer-window) cursor-in-echo-area)))

(defun flymake-cursor-show-stored-errors-now ()
  "Displays the stored error in the minibuffer."
  (interactive)
  (when flymake-cursor-mode
    (flymake-cursor-cancel-error-display-timer)
    (when flymake-cursor-errors-at-point
      (if (flymake-cursor-safe-to-display)
        (message "%s" (mapconcat 'flymake-cursor-pyflake-determine-message flymake-cursor-errors-at-point "\n"))
        (flymake-cursor-show-errors-at-point-pretty-soon)))))

(defun flymake-cursor-show-errors-at-point-now ()
  "If the cursor is sitting on a flymake error, display
the error message in the minibuffer."
  (interactive)
  (when flymake-cursor-mode
    (flymake-cursor-cancel-error-display-timer)
    (setq flymake-cursor-errors-at-point (flymake-cursor-get-errors-at-point))
    (if flymake-cursor-errors-at-point
      (flymake-cursor-show-stored-errors-now)
      ;; If something is demanding we display errors immediately, we do
      ;; want to clear the message area to indicate there's no errors.
      ;; Otherwise flymake-cursor-after-syntax-check will just keep the
      ;; old error for the current line if it has been corrected.
      (when (flymake-cursor-safe-to-display)
        (message nil)))))

(defun flymake-cursor-cancel-error-display-timer ()
  "Cancels `flymake-cursor-error-display-timer'."
  (when flymake-cursor-error-display-timer
    (cancel-timer flymake-cursor-error-display-timer)
    (setq flymake-cursor-error-display-timer nil)))

(defun flymake-cursor-show-errors-at-point-pretty-soon ()
  "If the cursor is sitting on a flymake error, grab the error,
and set a timer for \"pretty soon\". When the timer fires, the error
message will be displayed in the minibuffer.
The interval before the timer fires can be customized in the variable
`flymake-cursor-error-display-delay'.
This allows a post-command-hook to NOT cause the minibuffer to be
updated 10,000 times as a user scrolls through a buffer
quickly. Only when the user pauses on a line for more than a
second, does the flymake error message (if any) get displayed."
  (flymake-cursor-cancel-error-display-timer)
  (setq flymake-cursor-errors-at-point (flymake-cursor-get-errors-at-point))
  (when flymake-cursor-errors-at-point
    (setq flymake-cursor-error-display-timer
      (run-at-time flymake-cursor-error-display-delay nil 'flymake-cursor-show-stored-errors-now))))

(defun flymake-cursor-follow-flymake-mode ()
  "Hook function to make `flymake-cursor-mode` follow the on/off
status of `flymake-mode'."
  (if flymake-mode
    (when flymake-cursor-auto-enable (flymake-cursor-mode 1))
    (flymake-cursor-mode 0)))

(defun flymake-cursor-after-syntax-check ()
  "Run from `flymake-after-syntax-check-hook' to update our errors."
  (when (eq (current-buffer) (window-buffer))
    (flymake-cursor-show-errors-at-point-now)))

(eval-after-load "flymake"
  '(progn
    (if (boundp 'flymake-goto-error-hook)
      (add-hook 'flymake-goto-error-hook 'flymake-cursor-show-errors-at-point-now)
      (defadvice flymake-goto-line (after flymake-cursor-display-message-after-move-to-error activate compile)
        "Display the error in the mini-buffer rather than having to mouse over it"
         (flymake-cursor-show-errors-at-point-now)))
    (if (boundp 'flymake-after-syntax-check-hook)
      (add-hook 'flymake-after-syntax-check-hook 'flymake-cursor-after-syntax-check)
      (defadvice flymake-post-syntax-check (after flymake-cursor-display-message-after-syntax-check activate compile)
        "Display the error in the mini-buffer rather than having to mouse over it"
        (flymake-cursor-after-syntax-check)))
    (add-hook 'flymake-mode-hook 'flymake-cursor-follow-flymake-mode)))

(use-package flymake
  :after
  (eglot)
  :config
  (flymake-cursor-mode))
