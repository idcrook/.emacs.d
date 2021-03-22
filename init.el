;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;; from https://github.com/rakanalh/emacs-bootstrap
;;    / http://emacs-bootstrap.com

;;; Code:

;;______________________________________________________________________
;;;;  package management related


;;____________________________________________________________________________
;;;;  straight.el
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
;;----------------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
;;; https://github.com/raxod502/straight.el#updating-recipe-repositories
;;
;; updating a recipe repository (e.g. melpa) to get a newish PACKAGE-NAME
;;
;;     M-x straight-pull-package   melpa
;;     M-x straight-use-package    PACKAGE-NAME

;; set load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;; ----------------------------------------------------------------------------
;; Make startup faster by reducing the frequency of garbage collection, and
;; restore smaller value after startup.  Default is 800 kilobytes.  Measured in
;; bytes.
(let ((normal-gc-cons-threshold (* 800 1024))
      (init-gc-cons-threshold (* 500 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; https://blog.d46.us/advanced-emacs-startup/
;; Use a hook so the message doesn't get clobbered by other messages.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;; ;; ad-handle-definition: ‘align-regexp’ got redefined
;; ;; ad-handle-definition: ‘find-tag-regexp’ got redefined
(setq ad-redefinition-action 'accept)

(require 'base)                ;; basic setup
(require 'base-theme)          ;; pick themes
(require 'base-functions)      ;; utility functions
(require 'base-platforms)      ;; handle macOS, Linux, etc.
(require 'base-extensions)     ;; bulk of config not found below
(require 'base-global-keys)    ;; add bindings for functions

(require 'base-lsp)            ;; Language Server Protocol
(require 'lang-shell)          ;; shell-related (zsh, eshell, bash, etc)
(require 'lang-python)
(require 'lang-perl)
(require 'lang-ruby)
(require 'lang-go)
(require 'lang-elixir)
(require 'lang-apple)
(require 'lang-cpp)
(require 'lang-rust)
(require 'lang-javascript)
(require 'lang-web)
(require 'lang-hardware)

(require 'base-finally)  ;; any final cleanup

(provide 'init)

;;; init.el ends here
