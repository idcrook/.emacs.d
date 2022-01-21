;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; ;; the following is for previous versions of emacs; the early-init.el file
;; ;; is not even loaded in versions before 27 however, and package-initialize
;; ;; would be in init.el; keeping here (commented out) for historical reasons
;; (when (< emacs-major-version 27)
;;   (package-initialize))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;; ----------------------------------------------------------------------------
;; Make startup faster by reducing the frequency of garbage collection, and
;; restore smaller value after startup.  Emacs default is 800,000 bytes.
;; Measured in bytes.
(defvar normal-gc-cons-threshold (* 800 1024)
  "The post-init value to use for `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

(let ((init-gc-cons-threshold (* 500 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            #'(lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; https://blog.d46.us/advanced-emacs-startup/
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          #'(lambda ()
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)))

;; ;; use as debug aid: backtrace to show what is loading org
;; (eval-after-load "org" '(debug))
;; emacs28's included org has bug so can use to find early load triggers that
;; pull in built-in org
;; - magit-todos
;; - org-bullets


;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
