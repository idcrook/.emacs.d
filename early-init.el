;;; early-init.el --- Emacs 27+ pre-initialisation config

;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; ;;;;  package.el - so package-list-packages includes them
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "https://melpa.org/packages/"))

;; ;; the following is for previous versions of emacs; the early-init.el file
;; ;; is not even loaded in versions before 27 however, and package-initialize
;; ;; would be in init.el; keeping here (commented out) for historical reasons
;; (when (< emacs-major-version 27)
;;   (package-initialize))

;; So we can detect this having been loaded
(provide 'early-init)

;;; early-init.el ends here
