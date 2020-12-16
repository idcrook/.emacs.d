;;; lang-shell.el --- Shell script (sh, BASH, zsh)

;;; Commentary:

;; SEE ALSO: base-extensions.el

;;; Code:


;;; https://github.com/manateelazycat/aweshell
;;; https://github.com/zwild/eshell-prompt-extras
;;; https://github.com/peterwvj/eshell-up
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


;; zsh support (in sh-mode)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(add-hook 'sh-mode-hook
          (lambda ()
            (if (string-match "\\.zsh$" buffer-file-name)
                (sh-set-shell "zsh"))))


;;; https://github.com/Alexander-Miller/company-shell
(use-package company-shell
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env)))

;; https://opensource.apple.com/source/tcsh/tcsh-27.1/tcsh/csh-mode.el
;; installed manually/locally into ~/.emacs.d/user_elisp/csh-mode.el
(require 'csh-mode)

;; # https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :init
  ;; Its (original) value is '("PATH" "MANPATH")
  ;;(setq-default exec-path-from-shell-variables '("PATH" "GOPATH" "PYTHONPATH"))

  ;; Its (original) value is '("-l" "-i")
  ;; do not run an interactive shell (faster execution)
  (setq-default exec-path-from-shell-arguments '("--login" ))

  :config
  ;; Add GOPATH and PYTHONPATH to emacs shell
  (when (memq window-system '(mac ns x))
    (setq exec-path-from-shell-debug +1)
    ;; ;; only warn if delay exceeds 1000 ms
    ;; (setq exec-path-from-shell-warn-duration-millis 1000)
    (exec-path-from-shell-initialize))
  )


(provide 'lang-shell)

;;; lang-shell.el ends here
