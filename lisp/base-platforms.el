;;; base-platforms.el --- platform configs
;;; Commentary:
;;; Contains my platform configs

;;; Code:


;; use straight.el for https://github.com/idcrook/sysinfo
(use-package sysinfo
  :straight (:type git :host github :repo "idcrook/sysinfo"))

(defconst platform-windows-p
    (eq sysinfo-os-family 'Windows)
  "Are we running on a Windows system?")

;; https://emacs.stackexchange.com/questions/32079/how-do-i-test-if-my-emacs-is-in-mingw-environment#comment49373_32079
(defconst platform-msys2-p
  (and
   ;; MINGW64, e.g.
   (string-prefix-p "MINGW" (getenv "MSYSTEM"))
   (or platform-windows-p ()))
  "Are we running under MSYS2?")

(defconst platform-wsl-p
  (eq sysinfo-os-type 'WSL)
  "Are we running under WSL?")

(defconst platform-cygwin-p
  (eq sysinfo-os-type 'cygwin)
  "Are we running under cygwin?")

(defconst platform-linux-p
  (eq sysinfo-os-type 'Linux)
  "Are we running on a Linux system?")

(defconst platform-macos-p
    (eq sysinfo-os-family 'macOS)
  "Are we running on a macOS system?")

(defconst platform-linux-x-p
  (and
   platform-linux-p
   (eq window-system 'x))
  "Are we running on X Window System under Linux?")


;;______________________________________________________________________
;;;;  Fonts
;; macos :
;;       : brew cask install font-cascadia-mono-pl
;;       : brew cask install font-dejavusansmono-nerd-font
;;       : brew cask install font-hack-nerd-font
;;       : brew cask install font-inconsolata-for-powerline
;;       : brew cask install font-inconsolata-nerd-font
;;       : brew cask install font-menlo-for-powerline
;;       : brew cask install font-meslo-for-powerline
;;       : brew cask install font-ubuntumono-nerd-font
;; ubuntu
;;        : sudo apt install fonts-powerline
;;        : sudo apt install fonts-hack
;;        : sudo apt install fonts-ubuntu
;;        : sudo apt install fonts-inconsolata
;;        : sudo apt install fonts-dejavu
;;        : sudo apt install fonts-cascadia-code
;;      Emoji
;;        : sudo apt install fonts-symbola
;;        : sudo apt install ttf-ancient-fonts


;;; https://github.com/purcell/default-text-scale
;; Change global font size easily
;; "C-M-=" - decrease font size
;; "C-M--" - increase font size
;; "C-M-0" - reset
(use-package default-text-scale
  :init
  (add-hook 'after-init-hook 'default-text-scale-mode))

;; my various font family variables
(defvar  dpc-font-default)
(defvar  dpc-font-variable)
(defvar  dpc-font-modeline)

(setq
 ;; "powerline" fonts override plain font names in Linux
 ;;dpc-font-default "Hack"
 ;;dpc-font-default "Bitstream Vera Sans Mono"
 dpc-font-default "Inconsolata"
 ;;dpc-font-default "Roboto Mono"
 dpc-font-variable "Ubuntu Mono"
 dpc-font-modeline "DejaVu Sans Mono")

;; fonts appear with slightly different names on macOS than Ubuntu/Debian
(when platform-macos-p
  (setq
;;   dpc-font-default "Inconsolata for Powerline"
   dpc-font-default "Inconsolata Nerd Font"
;;   dpc-font-variable "Meslo for Powerline"
   dpc-font-variable "UbuntuMono Nerd Font"
;;   dpc-font-modeline "Cascadia Mono PL"
;;   dpc-font-modeline "Menlo for Powerline"
   dpc-font-modeline "DejaVuSansMono Nerd Font"
  ))

(defun dpc-setup-main-fonts (default-height variable-pitch-height modeline-height)
  "Set up default fonts.

Use DEFAULT-HEIGHT for default face, VARIABLE-PITCH-HEIGHT for
variable-pitch face, and MODELINE-HEIGHT for mode-line face."
  (set-face-attribute 'default nil
                      :family dpc-font-default
                      :height default-height
                      :weight 'regular)
  (set-face-attribute 'variable-pitch nil
                      :family dpc-font-variable
                      :height variable-pitch-height
                      :weight 'regular)
  (set-face-attribute 'mode-line nil
                      :family dpc-font-modeline
                      :height modeline-height
                      :weight 'regular))

;; Starting defaults
(set-face-attribute 'default nil
                    :family dpc-font-default
                    :height 160
                    :weight 'normal)
(set-face-attribute 'mode-line nil :family dpc-font-modeline :height 140 :weight 'regular)

;; adapt font sizes to display resolution - should probably consult
;; (x-display-mm-width)
;; (x-display-mm-height)
;; macOS ;; 2 X 27" SBS: 1413 mm
;;       ;;  2560x1440p:  397 mm

(when (display-graphic-p)
  (if (> (x-display-pixel-width) 1800)
      (if ;; very large number of pixels in display. side-by-side?
          (and
           (>= (x-display-pixel-width) 5120)
           (< (x-display-pixel-width) 6400))
	      (dpc-setup-main-fonts 160 160 140)
        (if (or
             (and             ;; specific display
              (= (x-display-pixel-width) 2560)
              (= (x-display-pixel-height) 1600))
             (and ;; another specific display setup
              (= (x-display-pixel-width) 3840)
              (= (x-display-pixel-height) 1080))
             )
            ;; (dpc-setup-main-fonts 140 140 120)
            (dpc-setup-main-fonts 160 160 140)
          (if (or
               (and ;; another specific display setup
                (= (x-display-pixel-width) 1920)
                (= (x-display-pixel-height) 1080))
               (and ;; another specific display setup
                (= (x-display-pixel-width) 5120)
                (= (x-display-pixel-height) 1440)))
              (dpc-setup-main-fonts 120 120 110)
            ;; other large display
	        (dpc-setup-main-fonts 180 180 160)))
        )
	(dpc-setup-main-fonts 140 140 120)))

;; can get hostname/display names
;; (x-display-list) ("w32")

;;; transparency ;;
(set-frame-parameter (selected-frame) 'alpha '(100 95))
(add-to-list 'default-frame-alist '(alpha 100 95))

;; (setq focus-follows-mouse t)
;; (setq focus-follows-mouse 'auto-raise)

;; turn menu bar off unless we are in a graphical display
(if (display-graphic-p) nil
  (menu-bar-mode -1))

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;  - start the emacs server (under X or macOS)
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (if (display-graphic-p)
                (unless (server-running-p)
                  (server-start)))))

;;; [dunn/company-emoji: company-mode backend for emoji](https://github.com/dunn/company-emoji#emoji-font-support)
(defun --set-emoji-font (frame)
  "Adjust the font settings of FRAME so Emacs can display emoji properly."
  (if (eq system-type 'darwin)
      ;; For NS/Cocoa
      ;;; https://www.reddit.com/r/emacs/comments/8ph0hq/i_have_converted_from_the_mac_port_to_the_ns_port/
      (if (version< "27.0" emacs-version)
          ;; not tested with emacs26 (requires a patched Emacs version for multi-color font support)
          (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
    ;; For Linux
    (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))

(cond
 ;;---------------------------------------------------------------------------
 ;; Windows-specific code goes here.
 ;;---------------------------------------------------------------------------
 (platform-windows-p)
  ;; ;;(require 'w32shell)
  ;; (setq w32shell-add-emacs-to-path t)
  ;; (setq w32shell-cygwin-bin "C:\\cygwin\\bin")
  ;; (setq w32shell-shell (quote cygwin))
  ;; (setq emacsw32-style-frame-title t))

 ;;---------------------------------------------------------------------------
 ;; macOS-specific code goes here.
 ;;---------------------------------------------------------------------------
 (platform-macos-p
  ;;; https://www.reddit.com/r/emacs/comments/4pocdd/advice_on_setting_up_emacs_on_os_x/d4ng534
  (setq mouse-wheel-scroll-amount '(2 ((shift) .1) ((control) . nil)))
  (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
  (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
  (setq scroll-step 1) ;; keyboard scroll one line at a timer
  ;; (require 'browse-url)
  ;; use system browser
  (setq browse-url-browser-function (quote browse-url-generic))
  (setq browse-url-generic-program "open")
  ;; (setq browse-url-browser-function (quote browse-url-default-macos-browser))
  ;; always use find-file-other-window to open dropped files
  (setq dnd-open-file-other-window t)
  ;;; Useful for https://github.com/dunn/company-emoji
  (--set-emoji-font nil)
  (add-hook 'after-make-frame-functions '--set-emoji-font)
  ;; TODO: introduce a boolean switch for setting among macOS native or windows keyboards and bindings below
  ;;; https://stackoverflow.com/questions/45697790/how-to-enter-special-symbols-with-alt-in-emacs-under-mac-os-x
  ;; use the left alt/option key as meta
  (setq ns-alternate-modifier 'meta) ; Its default value is ‘meta’
  (setq ns-command-modifier 'super) ; Its default value is ‘super’
  ;;(setq ns-option-modifier nil)   ; alias for ns-alternate-modifier
  ;;  'none: Do not have emacs capture (right-side) key for:
  ;; (setq ns-right-command-modifier 'none)   ;; original value is 'left'
  ;; setting to 'none allows "alternate" keyboard using a windows keyboard
  (setq ns-right-alternate-modifier 'none) ;; original value is 'left' -
  ;; can have emacs eat right alt/option key and command keys by setting to nil
  ;;(setq ns-right-command-modifier nil)   ;; original value is 'left'
  ;;(setq ns-right-alternate-modifier nil) ;; original value is 'left'
  ;;(setq ns-right-option-modifier nil)    ;; alias for ns-right-alternate-modifier
  ;;(setq ns-function-modifier 'none) ; Its value is ‘none’
  ;;(setq ns-function-modifier 'hyper) ; Its value is ‘none’
  ;; On macOS, if you are using a PC keyboard, the ▤ Menu (aka Apps) key will send [Ctrl+p] by default.
  ;; On windows, w32-apps-modifier is available
  ;; You can observe this with `C-h k` and then tapping the key
  ;; on macOS rebind this default key to the Menu key
  (define-key key-translation-map (kbd "C-p") (kbd "<menu>")) ;; <menu> runs counsel-M-x (found in counsel-mode-map),
  ;;--------------------------------------------------------------------------
  ;; dired conf
  ;;--------------------------------------------------------------------------
  (require 'dired)
    ;; Prefer g-prefixed coreutils version of standard utilities when available
  ;; requires Homebrew coreutils package;
  (let ((gls (executable-find "gls")))
    (when gls (setq insert-directory-program gls)))
  ;;; http://pragmaticemacs.com/emacs/automatically-copy-text-selected-with-the-mouse/
  ;; copy selected text to clipboard
  (setq mouse-drag-copy-region t)
  (add-to-list 'default-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 80))
  ;; macos titlebar mods
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;;(setq frame-title-format nil)
  ;; transparency
  (set-frame-parameter (selected-frame) 'alpha '(100 88))
  (add-to-list 'default-frame-alist '(alpha 100 88))
  (when  (fboundp 'toggle-frame-fullscreen)
    ;; Command-Option-f to toggle fullscreen mode
    ;; Hint: Customize `ns-use-native-fullscreen'
    (global-set-key (kbd "M-s-f") 'toggle-frame-fullscreen)))

 ;;---------------------------------------------------------------------------
 ;; X/Window under Linux code here
 ;;---------------------------------------------------------------------------
 (platform-linux-x-p
  ;; make alt meta (for silly X-Window remapping)
  ;; https://www.emacswiki.org/emacs/MetaKeyProblems#toc9
  (setq x-alt-keysym 'meta)
  (setq select-enable-clipboard t)
  ;;; via https://github.com/dunn/company-emoji
  ;; For when Emacs is started in GUI mode:
  (--set-emoji-font nil)
  ;; Hook for when a frame is created with emacsclient
  ;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
  (add-hook 'after-make-frame-functions '--set-emoji-font)
  ;; Treat clipboard input as UTF-8 string first; compound text
  ;; next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

 ;;---------------------------------------------------------------------------
 ;; Linux-specific code goes here.
 ;;---------------------------------------------------------------------------
 (platform-linux-p
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 100)))
 )

(provide 'base-platforms)

;;; base-platforms.el ends here
