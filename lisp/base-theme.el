;;; base-theme.el --- Themes for graphical and terminal sessions
;;
;;; Commentary:

;;; Code:

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; Trial themes
;;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;;(use-package color-theme-sanityinc-tomorrow)
;; sanityinc-tomorrow-blue is like Wordperfect
;; sanityinc-tomorrow-bright is actually a dark theme
;;; https://github.com/n3mo/cyberpunk-theme.el
;; (use-package cyberpunk-theme)
;; ;;; https://github.com/NicolasPetton/zerodark-theme
;; (use-package zerodark-theme)
;;    ;; :config
;;    ;; ;; Optionally setup the modeline
;;    ;; (zerodark-setup-modeline-format)
;;   )
;; ;;; https://github.com/mgrbyte/emacs-abyss-theme
;; (use-package abyss-theme)
;; ;;; https://github.com/osener/emacs-afternoon-theme
;; (use-package afternoon-theme)
;;; https://github.com/dracula/dracula-theme
;;; https://draculatheme.com/emacs/
;; (use-package dracula-theme)
;; ;;; https://github.com/rexim/gruber-darker-theme
;; (use-package gruber-darker-theme)
;;; https://github.com/alezost/alect-themes
;; (use-package alect-themes)
;; (setq alect-display-class '((class color) (min-colors 256)))

;;; [Modus Themes (Modus Operandi and Modus Vivendi) | Protesilaos Stavrou](https://protesilaos.com/modus-themes/#h:e979734c-a9e1-4373-9365-0f2cd36107b8)
(use-package modus-themes
  :init
  ;; Add all your customizations prior to loading the themes
  (setq         ;; modus-themes-bold-constructs nil
                ;; modus-themes-slanted-constructs t
                modus-themes-bold-constructs t
                modus-themes-mode-line '(3d accented))
  ;; Load the theme files before enabling a theme (else you get an error).
  (modus-themes-load-themes)
  ;; :config
  ;; Load the theme of your choice:
  ;;(modus-themes-load-operandi) ;; light
  ;;(modus-themes-load-vivendi)  ;; dark
  ;;  :bind ("<f5>" . modus-themes-toggle)
)

;; ;;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
;; ;; FIXME: handle light/dark mode
;; (defun my/apply-theme (appearance)
;;   "Load theme, taking current system APPEARANCE into consideration."
;;   (mapc #'disable-theme custom-enabled-themes)
;;   (pcase appearance
;;     ('light (load-theme 'tango t))
;;     ('dark (load-theme 'tango-dark t))))
;; (add-hook 'ns-system-appearance-change-functions #'my/apply-theme)

(defun dpc-load-theme ()
    "Set current theme, with preferences on GUI or terminal."
  (interactive)
  ;; use a different theme, depending on graphical window or (tty/console)
  (if (display-graphic-p)
      ;; (load-theme 'abyss 'no-confirm)
      ;; (load-theme 'zerodark 'no-confirm)
      ;; (load-theme 'afternoon 'no-confirm)
      ;; (load-theme 'dracula 'no-confirm)
      ;; (load-theme 'gruber-darker 'no-confirm)
      ;; Preferred ;; (load-theme 'sanityinc-tomorrow-bright 'no-confirm)
      (modus-themes-load-vivendi)
    ;; tty
    (load-theme 'tango-dark)  ;; tango-dark included in emacs
    ;;  (load-theme 'misterioso)  ;;
    ;;  (load-theme 'wheatgrass)  ;;
    ;;  (load-theme 'cyberpunk 'no-confirm)
    ))

(add-hook 'after-init-hook #'dpc-load-theme)

;;; M-x describe-face fill-column-indicator
;; (set-face-attribute 'fill-column-indicator nil :foreground "grey10")

;;----------------------------------------------------------------------------
;; powerline mode-line
;;----------------------------------------------------------------------------
;;; https://github.com/TheBB/spaceline
(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)  ;; (spaceline-emacs-theme)
  ;; segments
  (spaceline-toggle-buffer-position-on) ;; "The current approximate buffer position, in percent."
  (spaceline-toggle-buffer-size-off) ;; size of buffer
  (spaceline-toggle-buffer-id-on) ;; name of buffer
  (spaceline-toggle-remote-host-on) ;; Hostname for remote buffers
  (spaceline-toggle-line-column-on) ;; current line and column numbers
  (spaceline-toggle-selection-info-on) ;; info on currently active selection, if any
  (spaceline-toggle-hud-off) ;; shows currently visible part of buffer
  (spaceline-toggle-buffer-encoding-abbrev-off) ;; line ending convention (dos, mac, unix)
  (spaceline-toggle-buffer-encoding-off) ;; encoding type
  (spaceline-toggle-version-control-on) ;; version control information
  ;; (spaceline-toggle-projectile-root-on)
  (spaceline-toggle-window-number-on)       ;; window numbers
  (setq spaceline-window-numbers-unicode t) ;; see also: winum
  ;;(setq spaceline-highlight-face-func 'spaceline-highlight-face-default) ;; orange, all the time
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-modified) ;; based on (unmodified, modified, RO)
  ;; default ;; (setq spaceline-minor-modes-separator "|")
  )


(provide 'base-theme)

;;; base-theme.el ends here
