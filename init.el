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
;; customizing check variable to speedup emacs startup
(setq straight-check-for-modifications '(check-on-save find-when-checking))
;;(setq straight-check-for-modifications 'nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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

;;; https://andrewjamesjohnson.com/suppressing-ad-handle-definition-warnings-in-emacs/
;; ;; ad-handle-definition: ‘align-regexp’ got redefined
;; ;; ad-handle-definition: ‘find-tag-regexp’ got redefined
(setq ad-redefinition-action 'accept)

;; https://emacs.stackexchange.com/questions/74289/emacs-28-2-error-in-macos-ventura-image-type-invalid-image-type-svg#comment127558_74801
;; See https://debbugs.gnu.org/cgi/bugreport.cgi?bug=59081
(if (version< emacs-version "29.0")
    ;; alias to other function
    (add-to-list 'image-types 'svg)
  ;; should be fixed in emacs 29
  )



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
(require 'lang-lisp)
(require 'lang-web)
(require 'lang-hardware)
(require 'lang-other)

(require 'base-finally)  ;; any final cleanup

(provide 'init)

;;; init.el ends here
