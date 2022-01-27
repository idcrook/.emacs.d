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

(defconst platform-wsl-pgtk-p
  (and
   platform-wsl-p
   (eq window-system 'pgtk))
  "Are we running under WSL with pGTK build?")

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

(defun get-default-height ()
  "Calculates number of characters tall the display is, assuming size 12 font?"
       (/ (- (display-pixel-height) 120)
          (frame-char-height)))

(defun dpc-frame-height-default ()
  "Set height in `default-frame-alist'."
  (interactive)
  (if platform-wsl-pgtk-p
      ;; hardcode until emacs29/WSL pgtk wayland way is determined
      (add-to-list 'default-frame-alist '(height . 52))
    (add-to-list 'default-frame-alist (cons 'height (get-default-height)))))

;;(add-hook 'after-init-hook 'dpc-frame-height-default)
(add-hook 'emacs-startup-hook 'dpc-frame-height-default)

;; (print (font-family-list))
;; (message (string-join (font-family-list) "\n"))
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
;;        : sudo apt install fonts-anonymous-pro
;;        : sudo apt install fonts-dejavu
;;        : sudo apt install fonts-cascadia-code
;;      Emoji
;;        : sudo apt install fonts-symbola
;;        : sudo apt install ttf-ancient-fonts

;; ubuntu WSLg pgtk
;;        : sudo apt install fonts-cascadia-code
;;        : sudo apt install fonts-powerline
;;        : sudo apt install fonts-inconsolata
;;        : sudo apt install fonts-ubuntu
;;      Emoji
;;        : sudo apt install ttf-ancient-fonts-symbola

;; ;; Change global font size easily
;; ;;; https://github.com/purcell/default-text-scale
;; ;; "C-M-=" - decrease font size
;; ;; "C-M--" - increase font size
;; ;; "C-M-0" - reset
;; (use-package default-text-scale
;;   :init
;;   (add-hook 'after-init-hook 'default-text-scale-mode))

(defun dpc-font-available-p (font-name)
  "Check whether FONT-NAME is available from system."
  (member font-name (font-family-list)))

;; Declare various font family variables
;; "powerline" fonts override their "plain" font names in Linux
(defvar
  dpc-font-frame-default "Inconsolata"
  "The default font to use for frames.")
(defvar
  dpc-font-default "Inconsolata"
  "The default font to use.")
(defvar
  dpc-font-variable "Ubuntu Mono"
  "The font to use for variable-width faces.")
(defvar
  dpc-font-modeline "DejaVu Sans Mono"
  "The font to use for modeline.")

;; priority order based on availability
(when platform-linux-x-p
  (cond
   ((dpc-font-available-p "Inconsolata Nerd Font Mono")
    (setq dpc-font-frame-default "Inconsolata Nerd Font Mono"))
   ((dpc-font-available-p "Cascadia Mono PL")
    (setq dpc-font-frame-default "Cascadia Mono PL"))
   ((dpc-font-available-p "Inconsolata")
    (setq dpc-font-frame-default "Inconsolata"))
    ))

;; fonts appear with slightly different names on macOS than Ubuntu/Debian
(when platform-macos-p
  (when (dpc-font-available-p "Inconsolata Nerd Font")
    (setq dpc-font-frame-default "Inconsolata Nerd Font"))
  (when (dpc-font-available-p "Cascadia Code")
    (setq dpc-font-frame-default "Cascadia Code"))
  (setq
   dpc-font-default "Inconsolata Nerd Font"
   dpc-font-variable "UbuntuMono Nerd Font"
   dpc-font-modeline "DejaVuSansMono Nerd Font")
  )

;; fonts appear with slightly different names on macOS than Ubuntu/Debian
(when platform-wsl-pgtk-p
  (cond
   ((dpc-font-available-p "Cascadia Mono PL")
    (setq dpc-font-frame-default "Cascadia Mono PL")))
  (setq
   dpc-font-default "Inconsolata"
   dpc-font-variable "Ubuntu Mono"
   dpc-font-modeline "DejaVu Sans Mono"))


(defun dpc-setup-main-fonts (frame-default-height default-height variable-pitch-height modeline-height)
  "Set up default fonts.

Use FRAME-DEFAULT-HEIGHT for default frame font, DEFAULT-HEIGHT
for default face, VARIABLE-PITCH-HEIGHT for variable-pitch face,
and MODELINE-HEIGHT for mode-line face."
  ;; (message (format "setup-fonts: %d %d %d %d" frame-default-height default-height variable-pitch-height modeline-height nil t))
  (set-face-attribute 'default nil
                      :family dpc-font-default
                      :height default-height
                      :weight 'regular)
  ;; set frame font after default face attribute
  (set-frame-font
   (format "%s-%d" dpc-font-frame-default (/ frame-default-height 10)) nil t)
  (set-face-attribute 'variable-pitch nil
                      :family dpc-font-variable
                      :height variable-pitch-height
                      :weight 'regular)
  (set-face-attribute 'mode-line nil
                      :family dpc-font-modeline
                      :height modeline-height
                      :weight 'regular))

;; (dpc-setup-main-fonts 140 140 140 120)


;; App launch defaults
(set-face-attribute 'default nil
                    :family dpc-font-default
                    :height 160
                    :weight 'normal)
;; set frame font after default face attribute
(set-frame-font dpc-font-frame-default nil t)
(set-face-attribute 'variable-pitch nil
                    :family dpc-font-variable
                    :height 160
                    :weight 'normal)
(set-face-attribute 'mode-line nil :family dpc-font-modeline :height 140 :weight 'regular)

;; adapt font sizes
(when (display-graphic-p)
  (if (>= (x-display-pixel-width) 1920)
      (if (and ;; very large number of pixels in display width
           (> (x-display-pixel-width) 5120)
           (< (x-display-pixel-width) 6400))
	      (dpc-setup-main-fonts 160 160 160 140)
        (if (or
             (and ;; specific display 2560x1600
              (= (x-display-pixel-width) 2560)
              (= (x-display-pixel-height) 1600))
             (and ;; specific display (SBS 1080p)
              (= (x-display-pixel-width) 3840)
              (= (x-display-pixel-height) 1080))
             )
            (dpc-setup-main-fonts 160 160 160 140)
          (if (or
               (and ;; specific display 2560x1440
                (= (x-display-pixel-width) 2560)
                (= (x-display-pixel-height) 1440))
               (and ;; 1080p
                (= (x-display-pixel-width) 1920)
                (= (x-display-pixel-height) 1080))
               )
              (dpc-setup-main-fonts 140 140 140 120)
            (if (or
                 (and ;; (SBS 1440p 2X 2560x1440)
                  (= (x-display-pixel-width) 5120)
                  (= (x-display-pixel-height) 1440))
                 )
                (dpc-setup-main-fonts 160 160 160 140)
              ;; fall-thru: HD display wide or larger
	          (dpc-setup-main-fonts 180 180 180 160))))
        )
	(dpc-setup-main-fonts 140 140 140 120)))

;; should probably consult (x-display-mm-width) (x-display-mm-height)
;; macOS ;; 2 X 27" SBS: 1413 mm
;;       ;;  2560x1440p:  397 mm

;; can get hostname/display names
;; (x-display-list) ("w32") ("wayland-0")
;;
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
          ;; not tested with emacs26 (requires patched Emacs26 for multi-color font support)
          (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
        (set-fontset-font t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
    ;; For !darwin (Linux)
    (if (version< emacs-version "29.0.50") ;; is it still needed in emacs29?
        (set-fontset-font t 'symbol (font-spec :family "Symbola") frame 'prepend)))
  )

;; For when Emacs is started in GUI mode:
(--set-emoji-font nil)
;; Hook for when a frame is created with emacsclient
;; see https://www.gnu.org/software/emacs/manual/html_node/elisp/Creating-Frames.html
(add-hook 'after-make-frame-functions '--set-emoji-font)


(cond
 ;;---------------------------------------------------------------------------
 ;; Windows-specific code goes here.
 ;;---------------------------------------------------------------------------
 (platform-windows-p)

 ;;---------------------------------------------------------------------------
 ;; macOS-specific code goes here.
 ;;---------------------------------------------------------------------------
 (platform-macos-p
  ;;; https://www.reddit.com/r/emacs/comments/4pocdd/advice_on_setting_up_emacs_on_os_x/d4ng534
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 1) ((control) . nil)))
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
  ;; ;;; Useful for https://github.com/dunn/company-emoji
  ;; (--set-emoji-font nil)
  ;; (add-hook 'after-make-frame-functions '--set-emoji-font)
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
  ;;(add-to-list 'default-frame-alist '(height . 40))
  (add-to-list 'default-frame-alist '(width . 86))
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
 ;; WSL2/WSLg pgtk specific code goes here.
 ;;---------------------------------------------------------------------------
 (platform-wsl-pgtk-p
  ;; https://emacsredux.com/blog/2021/12/19/using-emacs-on-windows-11-with-wsl2/
  (defun copy-selected-text (start end)
    "A workaround for yank that shells out to clip.exe."
    (interactive "r")
    (if (use-region-p)
        (let ((text (buffer-substring-no-properties start end)))
          (shell-command (concat "echo '" text "' | clip.exe")))))
  (add-to-list 'default-frame-alist '(width . 86))
  )

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
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
  (add-to-list 'default-frame-alist '(width . 86))
  )

 ;;---------------------------------------------------------------------------
 ;; Linux-specific code goes here.
 ;;---------------------------------------------------------------------------
 (platform-linux-p
  )
 )

(provide 'base-platforms)

;;; base-platforms.el ends here
