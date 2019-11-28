;;; base-theme.el --- Themes for graphical and terminal sessions
;;
;;; Commentary:

;;; Code:

;; ;; https://github.com/mgrbyte/emacs-abyss-theme
;; (use-package abyss-theme
;;   :ensure t)


;;; https://github.com/NicolasPetton/zerodark-theme
(use-package zerodark-theme
   ;; :config
   ;; ;; Optionally setup the modeline
   ;; (zerodark-setup-modeline-format)
  )

;;; https://github.com/popcorn4dinner/darkplus-emacs
;; (straight-use-package '(darkplus :type git :host github :repo "popcorn4dinner/darkplus-emacs"))

;;; https://github.com/n3mo/cyberpunk-theme.el
(use-package cyberpunk-theme
  )

;; in non-window (tty/console) use a different theme (manoj-dark)
(if (display-graphic-p)
    ;; (load-theme 'abyss 'no-confirm)
    (load-theme 'zerodark 'no-confirm)
;;  (load-theme 'tango-dark))  ;; tango-dark included in emacs
  (load-theme 'cyberpunk))
;;  (load-theme 'wheatgrass))  ;;

;;; https://github.com/TheBB/spaceline
;; powerline mode-line
(use-package spaceline
  :init
  (require 'spaceline-config)
  :config
  ;; segments
  (spaceline-spacemacs-theme)
  ;; (spaceline-emacs-theme)
  (spaceline-toggle-buffer-size-off)
  (spaceline-toggle-buffer-id-on)
  (spaceline-toggle-remote-host-on)
  (spaceline-toggle-buffer-position-off)
  (spaceline-toggle-line-column-on)
  (spaceline-toggle-hud-off)
  (spaceline-toggle-projectile-root-on)
  (spaceline-toggle-window-number-on)
  ;; numbers
  (setq spaceline-window-numbers-unicode t)
  (setq spaceline-workspace-numbers-unicode t))


(provide 'base-theme)

;;; base-theme.el ends here
