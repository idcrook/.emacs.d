;;; lang-shell.el --- Shell script (sh, BASH, zsh)

;;; Commentary:

;; SEE ALSO: base-extensions.el

;;; Code:


;;; https://github.com/manateelazycat/aweshell
;;; https://github.com/zwild/eshell-prompt-extras
;;; https://github.com/peterwvj/eshell-up
(use-package aweshell
  :straight (:type git :host github
                   :repo "manateelazycat/aweshell")
  :init
  (with-eval-after-load "esh-opt"
    (autoload 'epe-theme-lambda "eshell-prompt-extras")
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda))
  :config
  (setq epe-path-style 'full)
  (setq eshell-up-ignore-case nil)
  (setq eshell-up-print-parent-dir t)
  )


;;; https://github.com/szermatt/emacs-bash-completion
(use-package bash-completion)


;; zsh support (in sh-mode)
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

(add-hook 'sh-mode-hook
          (lambda ()
            (if (string-match "\\.zsh$" buffer-file-name)
                (sh-set-shell "zsh"))))


;; https://opensource.apple.com/source/tcsh/tcsh-27.1/tcsh/csh-mode.el
;; installed manually/locally into ~/.emacs.d/user_elisp/csh-mode.el
(require 'csh-mode)

;; # https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  ;; :init
  ;; (setq exec-path-from-shell-check-startup-files nil)
  ;; setting this seems to have no effect
  :config
  ;; Add GOPATH and PYTHONPATH to emacs shell
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-initialize)))

;; ;; fish shell style completion
;; (use-package fish-completion
;;   :config
;;   (when (and (executable-find "fish")
;; 	         (require 'fish-completion nil t))
;;     (global-fish-completion-mode)))

;; ;; https://github.com/wwwjfy/emacs-fish
;; (use-package fish-mode)



(provide 'lang-shell)

;;; lang-shell.el ends here
