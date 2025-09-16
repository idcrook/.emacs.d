;;; base-theme.el --- Themes for graphical and terminal sessions
;;
;;; Commentary:

;;; Code:

;; Don't prompt to confirm theme safety. This avoids problems with
;; first-time startup on Emacs > 26.3.
(setq custom-safe-themes t)

;; ;;; https://protesilaos.com/modus-themes/
;; (use-package modus-themes
;;   ;;:straight (:type git :host github :repo "emacs-straight/modus-themes")
;;   :straight (:type git :host github :repo "protesilaos/modus-themes")
;;   )

;;; https://protesilaos.com/emacs/ef-themes
(use-package ef-themes
  ;;  :straight (:type git :host github :repo "emacs-straight/ef-themes")
  :straight (:type git :host github :repo "protesilaos/ef-themes")
  :init
  (ef-themes-take-over-modus-themes-mode 1)
  :bind
  (("<f5>" . modus-themes-rotate)
   ("C-<f5>" . modus-themes-select)
   ("M-<f5>" . modus-themes-load-random-dark))
  :config
  ;; Add all your customizations prior to loading the themes
  (setq
   ;;modus-themes-bold-constructs nil
   ;;modus-themes-slanted-constructs t
   modus-themes-italic-constructs t
   ;;modus-themes-bold-constructs t
   ;;modus-themes-mode-line '(3d accented)
   )
   (setq modus-themes-to-toggle '(ef-night ef-maris-dark))
  )


(defun dpc-load-theme ()
    "Set current theme, with preferences on GUI or terminal."
  (interactive)
  ;; use a different theme, depending on graphical window or (tty/console)
  (if (display-graphic-p)
      ;; Preferred ;; (load-theme 'sanityinc-tomorrow-bright 'no-confirm)
      ;; load the theme which also calls `ef-themes-post-load-hook':
      ;;(ef-themes-select 'ef-night)
      (modus-themes-select 'ef-night)

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
  ;; (spaceline-spacemacs-theme)
  (spaceline-emacs-theme)
  ;; segments
  (spaceline-toggle-buffer-position-off) ;; "The current approximate buffer position, in percent."
  (spaceline-toggle-buffer-size-off) ;; size of buffer
  (spaceline-toggle-buffer-id-on) ;; name of buffer
  (spaceline-toggle-remote-host-on) ;; Hostname for remote buffers
  (spaceline-toggle-line-column-on) ;; current line and column numbers
  (spaceline-toggle-selection-info-on) ;; info on currently active selection, if any
  (spaceline-toggle-hud-on) ;; shows currently visible part of buffer
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
