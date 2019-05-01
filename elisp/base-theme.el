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

;; in non-window (tty/console) use a different theme (manoj-dark)
(if (display-graphic-p)
    ;; (load-theme 'abyss 'no-confirm)
    (load-theme 'zerodark 'no-confirm)
  (load-theme 'wheatgrass))  ;; wheatgrass included in emacs

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
