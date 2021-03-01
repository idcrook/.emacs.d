;;; package --- Main init file
;;; Commentary:
;;; This is my init file

;; from https://github.com/rakanalh/emacs-bootstrap
;;    / http://emacs-bootstrap.com

;;; Code:

;;______________________________________________________________________
;;;;  package management related


;;______________________________________________________________________
;;;;  straight.el
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#getting-started
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

;; for faster emacs start-up; gets re-set later
(setq gc-cons-threshold (* 50 1000 1000))

;; set load path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;; ;; ad-handle-definition: ‘align-regexp’ got redefined
;; ;; ad-handle-definition: ‘find-tag-regexp’ got redefined
(setq ad-redefinition-action 'accept)

(require 'base)
(require 'base-theme)
(require 'base-functions)
(require 'base-platforms)
(require 'base-extensions)
(require 'base-global-keys)

(require 'lang-shell)

(require 'lang-python)

(require 'lang-ruby)

(require 'lang-go)

(require 'lang-elixir)

(require 'lang-apple)

(require 'lang-cpp)

(require 'lang-rust)

(require 'lang-javascript)

(require 'lang-web)

(require 'lang-hardware)

(require 'base-finally)

(provide 'init)

;;; init.el ends here
