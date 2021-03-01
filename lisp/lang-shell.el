;;; lang-shell.el --- Shell script (sh, BASH, zsh)

;;; Commentary:

;; SEE ALSO: base-extensions.el

;;; Code:


;; ;;; https://github.com/manateelazycat/aweshell
;; ;;; https://github.com/zwild/eshell-prompt-extras
;; ;;; https://github.com/peterwvj/eshell-up
;; (use-package aweshell
;;   :straight (:type git :host github
;;                    :repo "manateelazycat/aweshell")
;;   :init
;;   (with-eval-after-load "esh-opt"
;;     (autoload 'epe-theme-lambda "eshell-prompt-extras")
;;     (setq eshell-highlight-prompt nil
;;           eshell-prompt-function 'epe-theme-lambda))
;;   :config
;;   (setq epe-path-style 'full)
;;   (setq eshell-up-ignore-case nil)
;;   (setq eshell-up-print-parent-dir t)
;;   )

;; ;;; https://github.com/szermatt/emacs-bash-completion
;; (use-package bash-completion
;;   :config
;;   (bash-completion-setup))
;;
;; (defun bash-completion-from-eshell ()
;;   (interactive)
;;   (let ((completion-at-point-functions
;;          '(bash-completion-eshell-capf)))
;;     (completion-at-point)))
;;
;; (defun bash-completion-eshell-capf ()
;;   (bash-completion-dynamic-complete-nocomint
;;    (save-excursion (eshell-bol) (point))
;;    (point) t))


;;----------------------------------------------------------------------------
;; zsh support (using sh-mode)
;;----------------------------------------------------------------------------
;;; via https://stackoverflow.com/a/20561363/47850
;; can use modeline in file: # -*- mode: sh; eval: (sh-set-shell "zsh") -*-
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(add-hook 'sh-mode-hook
          (lambda ()
            (if (string-match "\\.zsh$" buffer-file-name)
                (sh-set-shell "zsh"))))

;;; https://github.com/Alexander-Miller/company-shell
(use-package company-shell
  :init
  (add-to-list 'company-backends '(company-shell company-shell-env)))

;;----------------------------------------------------------------------------
;; csh support - installed manually/locally into site-lisp/csh-mode.el
;;----------------------------------------------------------------------------
;;; https://opensource.apple.com/source/tcsh/tcsh-27.1/tcsh/csh-mode.el
(require 'csh-mode)

;;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :init
  ;; ;; Its (original) value is '("PATH" "MANPATH")
  ;; (setq-default exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))
  ;; one at time ;; (exec-path-from-shell-copy-env "PYTHONPATH")

  ;; Its (original) value is '("-l" "-i")
  ;; ;; do not run an interactive shell (faster)
  ;; (setq-default exec-path-from-shell-arguments '("--login" ))
  ;; do not run interactive or login shell
  (setq-default exec-path-from-shell-arguments nil)

  :config
  (when (memq window-system '(mac ns x))
    ;; ;; DEBUG
    ;; (setq exec-path-from-shell-debug +1)
    ;; ;; warn if delay exceeds 1000 ms
    ;; (setq exec-path-from-shell-warn-duration-millis 1000)
    (exec-path-from-shell-initialize))
  )


(provide 'lang-shell)

;;; lang-shell.el ends here
