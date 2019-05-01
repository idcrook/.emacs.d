;;; base-finally.el --- fixup code (run after all other startup files)

;;; Commentary:

;;; Code:

;; https://blog.d46.us/advanced-emacs-startup/
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

(provide 'base-finally)

;;; base-finally.el ends here
