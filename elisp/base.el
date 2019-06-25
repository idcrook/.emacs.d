;;; base.el --- most of built-in emacs config here
;;
;;; Commentary:
;;
;; https://github.com/rakanalh/emacs-bootstrap

;;; Code:


;; See ~/.emacs.d/user_elisp/README.md
(add-to-list 'load-path (concat user-emacs-directory "user_elisp"))

(defconst private-dir   (expand-file-name "private" user-emacs-directory)
  "Set aside per-user Emacs directory.")

(defconst temp-dir (format "%s/cache" private-dir)
  "Location of package bookkeeping temp directories.")

;; Core settings
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Emacs customizations
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq
 confirm-kill-emacs                  'y-or-n-p
 confirm-nonexistent-file-or-buffer  t
 save-interprogram-paste-before-kill t
 mouse-yank-at-point                 t
 require-final-newline               t
 visible-bell                        nil
 ring-bell-function                  'ignore
 custom-file                         "~/.emacs.d/custom.el"

 ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
 minibuffer-prompt-properties
 '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

 ;; alongside $PATH via exec-path-from-shell package
 exec-path                          (append exec-path '("/usr/local/bin/"))

 ;; I hate tabs!
 indent-tabs-mode                   nil

 ;; modeline and major mode defaults
 column-number-mode                 nil
 auto-compression-mode              1
 ;;;menu-bar-mode                      nil  ; see base-platforms

 blink-matching-paren               t

 ;; abbrev
 abbrev-file-name                   (concat temp-dir "/abbrev_defs")

 history-length                     1000

 ;;; searching ;;
 query-replace-highlight            t
 search-highlight                   t
 case-fold-search                   t

 ;; startup stuff - see dashboard package ;;
 fancy-splash-image                 nil
 inhibit-default-init               nil
 inhibit-startup-screen             t
 inhibit-startup-message            t

 ;; Disable non selected window highlight
 cursor-in-non-selected-windows     nil
 highlight-nonselected-windows      nil
 )

;; Emacs 26 removed the default-SOMETHING
(setq-default major-mode                 'text-mode
              fill-column                79)

;;; many of the variable-wrapper functions below are intended to be
;;; called directly; i.e. do not set the underlying variable but
;;; instead call the function

;;; global minor mode that reverts any buffer associated with a file
;;; when the file changes on disk
(require 'autorevert)
(global-auto-revert-mode 1)

;;; Bookmarks ;;
(require 'bookmark)
(setq bookmark-save-flag                 t
      bookmark-default-file              (concat temp-dir "/bookmarks"))

(require 'cc-vars)
(setq c-basic-offset                     2)

;; overwrite of selection
(require 'delsel)
(delete-selection-mode t)

(require 'eshell)

;; emacs 25 has a network security manager ;;
(require 'nsm)
(setq nsm-settings-file                  (concat temp-dir "/network-security.data"))

;; visualization of matching parens
(require 'paren)
(show-paren-mode 1)
;;(setq show-paren-style 'parenthesis) ; default
(setq show-paren-style 'expression)
;;(setq show-paren-style 'mixed)
;;(setq show-paren-delay 0)

;;; https://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq save-place-file (concat temp-dir "/emacs-places"))
(save-place-mode 1)

;; Timestamps
(require 'time-stamp)
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-pattern nil)

;; Delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; FIXME:  only append if not already present
;; (when (file-directory-p (expand-file-name "~/.local/bin"))
;;   (setq exec-path    (append exec-path (list (expand-file-name "~/.local/bin")))))

;; Line highlighting in all buffers
;;(global-hl-line-mode 1)

;; Echo area display of Lisp objects at point
;;(eldoc-mode 1)

;; Alias to shorten prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Some window/frame appearance settings
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'tooltip-mode)
  (tooltip-mode -1))

(setq uniquify-buffer-name-style 'post-forward)

; If images are supported then display them when visiting them
(when (fboundp 'auto-image-file-mode)
    (auto-image-file-mode 1))

;;;________________________________________________________________________
;; === Backup and save or autosave stuff

;; localize it for safety.
(make-variable-buffer-local 'backup-inhibited)

;; Backups
(setq
 make-backup-files   t
 backup-directory-alist            `((".*" . ,(concat temp-dir "/backup/")))
 backup-by-copying   t      ; don't clobber symlinks
 backup-by-copying-when-mismatch t
 backup-by-copying-when-linked   t
 create-lockfiles    nil
 delete-old-versions t
 kept-old-versions   1
 kept-new-versions   3
 version-control     t       ; use versioned backups
 ;; ;; Backups disabled
 ;; backup-inhibited                   t
 ;; make-backup-files                  nil
 )


;; autosaves

;; create the autosave dir if necessary, since emacs won't.
(make-directory (concat temp-dir "/autosaves/") t)

(setq
 auto-save-default   t
 auto-save-list-file-name           (concat temp-dir "/autosave")
 auto-save-list-file-prefix         (concat temp-dir "/autosaves/autosave-")
 auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/autosaves/\\1") t))
 ;;auto-save-file-name-transforms    `((".*" ,(concat temp-dir "/auto-save-list/") t))
 )

;;(unless (file-exists-p (concat temp-dir "/autosave-save-list"))
;;  (make-directory (concat temp-dir "/autosave-save-list") :parents))


;; For faster initial connection times, TRAMP stores previous
;; connection properties in this file
(require 'tramp)
(require 'tramp-cache)
(setq  tramp-persistency-file-name (expand-file-name "tramp" temp-dir))

;; see base-platforms.el for
;;  - emacs server (used by emacsclient)
;;  - platform specific fonts, keybindings, workarounds


(provide 'base)

;;; base.el ends here
