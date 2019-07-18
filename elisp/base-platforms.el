;;; base-platforms.el --- platform configs
;;; Commentary:
;;; Contains my platform configs

;;; Code:

(defconst win32-p
    (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")


;; https://emacs.stackexchange.com/questions/32079/how-do-i-test-if-my-emacs-is-in-mingw-environment#comment49373_32079
(defconst platform-msys2-p

  (and
   ;; MINGW64, e.g.
   (string-prefix-p "MINGW" (getenv "MSYSTEM"))
   (or (eq system-type 'windows-nt)
        ()))
  "Are we running under MSYS2?")

(defconst platform-wsl-p
  (and
   (string-match-p "Windows" (getenv "PATH"))
   (or (eq system-type 'gnu/linux)
        (eq system-type 'linux)))
  "Are we running under WSL?")

;; (defconst cygwin-p
;;     (eq system-type 'cygwin)
;;   "Are we running on a WinTel cygwin system?")

(defconst linux-p
    (or (eq system-type 'gnu/linux)
        (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")

(defconst macos-p
    (eq system-type 'darwin)
  "Are we running on a macOS system?")


(defconst linux-x-p
  (and window-system linux-p)
  "Are we running under X on a GNU/Linux system?")
;; window-system as boolean is deprecated

;;______________________________________________________________________
;;;;  Fonts
;; macos : brew cask install font-ubuntu-mono-derivative-powerline
;;       : brew cask install font-inconsolata-for-powerline
;;       : brew cask install font-dejavu-sans-mono-for-powerline
;; ubuntu : sudo apt-get install fonts-powerline
;;        : sudo apt-get install fonts-ubuntu
;;        : sudo apt-get install fonts-hack
;;        : sudo apt-get install fonts-dejavu

;; my various font family variables
(defvar  dpc-font-default)
(defvar  dpc-font-variable)
(defvar  dpc-font-modeline)

(setq
 ;; "powerline" fonts override plain font names in Linux
 dpc-font-default "Inconsolata"
 dpc-font-variable "Ubuntu Mono"
 dpc-font-modeline "DejaVu Sans Mono")

;; fonts appear with slightly different names on macOS than Ubuntu/Debian
(when (string= "darwin" system-type)
  (setq
   dpc-font-default "Inconsolata for Powerline"
   dpc-font-variable "Ubuntu Mono derivative Powerline"
   dpc-font-modeline "DejaVu Sans Mono for Powerline"))


(defun inconsolata ()
  "Set the default font to Inconsolata."
  (interactive)
  (if (string= "darwin" system-type)
      (set-frame-font "Inconsolata for Powerline")
    (set-frame-font "Inconsolata")))

(defun dpc-setup-main-fonts (default-height variable-pitch-height modeline-height)
  "Set up default fonts.

Use DEFAULT-HEIGHT for default face, VARIABLE-PITCH-HEIGHT for
variable-pitch face, and MODELINE-HEIGHT for mode-line face."
  (set-face-attribute 'default nil
                      :family dpc-font-default
                      :height default-height)
  (set-face-attribute 'variable-pitch nil
                      :family dpc-font-variable
                      :height variable-pitch-height
                      :weight 'regular)
  (set-face-attribute 'mode-line nil
                      :family dpc-font-modeline
                      :height modeline-height
                      :weight 'regular))

;; defaults
(set-face-attribute 'default nil
                    :family dpc-font-default
                    :height 160
                    :weight 'normal)
(set-face-attribute 'mode-line nil :family dpc-font-modeline :height 140 :weight 'regular)



;; adapt font sizes to display resolution - should probably consult
;; (x-display-mm-width)  ;; 1600
;; (x-display-mm-height) ;; 1000

(when (display-graphic-p)
  (if (> (x-display-pixel-width) 1800)
      (if ;; very large number of pixels in display. side-by-side?
          (> (x-display-pixel-width) 5000)
	      (dpc-setup-main-fonts 160 160 140)
        (if (or
             (and             ;; specific display
              (= (x-display-pixel-width) 2560)
              (= (x-display-pixel-height) 1600))
             (and ;; another specific display setup
              (= (x-display-pixel-width) 3840)
              (= (x-display-pixel-height) 1080)))
            (dpc-setup-main-fonts 140 140 120)

          ;; other large display
	      (dpc-setup-main-fonts 180 180 160)
          ))
	(dpc-setup-main-fonts 140 140 120)))


;; (x-display-list) ("w32")


;;; transparency ;;
(set-frame-parameter (selected-frame) 'alpha '(97 85))
(add-to-list 'default-frame-alist '(alpha 97 85))

;; (setq focus-follows-mouse t)
;; (setq focus-follows-mouse 'auto-raise)

;; turn menu bar off unless we are in a graphical display
(if (display-graphic-p) nil
  (menu-bar-mode -1))

;; start the emacs server (under X or Cocoa)
(use-package server
  :if (display-graphic-p)
  :commands (server-running-p server-start)
  :init
  (server-mode 1)
  :config
  (unless (server-running-p)
    (add-hook 'after-init-hook 'server-start t))
  (require 'org-protocol))

(cond (win32-p ;; Windows-specific code goes here.
       ;;(require 'w32shell)
       (setq w32shell-add-emacs-to-path t)
       (setq w32shell-cygwin-bin "C:\\cygwin\\bin")
       (setq w32shell-shell (quote cygwin))
       (setq emacsw32-style-frame-title t))
      (linux-p ;; Linux-specific code goes here.
       ;;; https://github.com/dunn/company-emoji
       ;; sudo apt install fonts-symbola
       (set-fontset-font t 'symbol (font-spec :family "Symbola") nil 'prepend))
      (macos-p ;; macOS-specific code goes here.
       ;;; Look into osx-lib https://github.com/raghavgautam/osx-lib

       ;;; https://www.reddit.com/r/emacs/comments/4pocdd/advice_on_setting_up_emacs_on_os_x/d4ng534
       (setq smooth-scroll-margin 2)
       (setq mouse-wheel-scroll-amount '(2 ((shift) .1) ((control) . nil)))
       (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

       (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
       (setq scroll-step 1) ;; keyboard scroll one line at a timer

       (require 'browse-url)
       ;; use system browser
       (setq browse-url-browser-function (quote browse-url-default-macosx-browser))
       (setq browse-url-firefox-new-window-is-tab t)
       (setq browse-url-new-window-flag t)

       ;; always use find-file-other-window to open dropped files
       (setq dnd-open-file-other-window t)

       ;;; Useful for https://github.com/dunn/company-emoji
       ;; https://www.reddit.com/r/emacs/comments/8ph0hq/i_have_converted_from_the_mac_port_to_the_ns_port/
       ;; not tested with emacs26 (requires a patched Emacs version for multi-color font support)
       (if (version< "27.0" emacs-version)
           (set-fontset-font
            "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
         (set-fontset-font
          t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))

       ;;; https://stackoverflow.com/questions/45697790/how-to-enter-special-symbols-with-alt-in-emacs-under-mac-os-x
       ;; use the left alt/option key as meta
       (setq ns-alternate-modifier 'meta) ; Its default value is ‘meta’
       ;;(setq ns-command-modifier 'meta) ; Its value is ‘super’
       ;;(setq ns-option-modifier nil)   ; alias for ns-alternate-modifier

       ;;  Do not have emacs capture right alt/option key and command keys
       (setq ns-right-command-modifier 'none)   ;; original value is 'left'
       (setq ns-right-alternate-modifier 'none) ;; original value is 'left'
       ;;  have emacs eat right alt/option key and command keys
       ;;(setq ns-right-command-modifier nil)   ;; original value is 'left'
       ;;(setq ns-right-alternate-modifier nil) ;; original value is 'left'
       ;;(setq ns-right-option-modifier nil)      ;; alias for ns-right-alternate-modifier

       ;;; dired conf ;;
       (require 'dired)
       ;; set a fallback
       (setq ls-lisp-use-insert-directory-program t)
       ;;; https://github.com/n3mo/.emacs.d/blob/master/init.el
       ;; (setq dired-use-ls-dired nil)
       ;;; https://github.com/abo-abo/swiper/issues/184#issuecomment-127513387
       ;; requires Homebrew coreutils package; installs GNU tools
       ;; prefixed with 'g' character
       (let ((gls "/usr/local/bin/gls"))
         (when (file-exists-p gls)
           (setq insert-directory-program gls)))
       ;; (setq insert-directory-program (executable-find "gls"))

       ;;; http://pragmaticemacs.com/emacs/automatically-copy-text-selected-with-the-mouse/
       ;; copy selected text to clipboard
       (setq mouse-drag-copy-region t)

       (when macos-p
         (add-to-list 'default-frame-alist '(height . 40))
         (add-to-list 'default-frame-alist '(width . 80))
         ;; macos titlebar mods
         (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
         (add-to-list 'default-frame-alist '(ns-appearance . dark))
         ;;(setq frame-title-format nil)
         ;; transparency
         (set-frame-parameter (selected-frame) 'alpha '(100 88))
         (add-to-list 'default-frame-alist '(alpha 100 88)))
       )
      (linux-x-p
       ;; make alt meta (for silly X-Window remapping)
       ;; https://www.emacswiki.org/emacs/MetaKeyProblems#toc9
       (setq x-alt-keysym 'meta)
       (setq select-enable-clipboard t)

       ;; Treat clipboard input as UTF-8 string first; compound text
       ;; next, etc.
       (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))))


(provide 'base-platforms)

;;; base-platforms.el ends here
