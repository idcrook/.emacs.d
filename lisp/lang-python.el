;;; package --- python configs
;;; Commentary:
;;; Contains my python configs

;;; Code:

;;; emacs elpy : https://github.com/jorgenschaefer/elpy
;; # code checks - flake8
;; # completion and code navigation - jedi
;; # automatic formatting - black autopep8 yapf
;; pip3 install --user --upgrade jedi flake8 autopep8 yapf black rope
;; pip3 install --user virtualenvwrapper
;; venv is now built into python3

;; TODO: Invetigate formatter https://github.com/raxod502/apheleia

(use-package python
  :init
   (setq python-shell-interpreter "python3")
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  ;;(dap-python-debugger 'debugpy)

  ;; (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  ;; (add-hook 'python-mode-hook #'my-python-mode-hook))
  ;; :config
  ;; (require 'dap-python))
  )

;; ;;; https://github.com/jorgenschaefer/elpy
;; (use-package elpy
;;   :init
;;   (setq elpy-rpc-python-command "python3")
;;   ;; do not enable elpy globally for python-mode; see my-python-mode-hook.
;;   ;; ;; lazy load elpy
;;   ;; (advice-add 'python-mode :before 'elpy-enable)
;;   :config
;;   (when (load "flycheck" t t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   ;; do not explicitly run; also, should only be run once per emacs session
;;   ;;(elpy-modules-global-init)
;;   )

;; ;; these are the default bindings
;; ;; :bind (:map elpy-mode-map1
;; ;; 	          ("M-." . elpy-goto-definition)
;; ;; 	          ("M-*" . pop-tag-mark))

;; (defun check-for-embedded-python ()
;;   (and (eq major-mode 'python-mode)
;;        (buffer-file-name)
;;        (string= (file-name-base (directory-file-name (file-name-directory (buffer-file-name))))
;;                 "CIRCUITPY")))

;; (defun  my-python-mode-hook ()
;;   (if (check-for-embedded-python)
;;       (progn
;;         (message "Detected embedded python")
;;         ;; (message  "%s" python-mode-hook) ;; elpy-mode should be removed from python-mode-hook after below enable
;;         ;; (interactive)
;;         ;; (elpy-modules-global-stop)
;;         (elpy-disable)
;;         (eldoc-mode -1)
;;         (flymake-mode -1)
;;         (flycheck-mode -1))
;;     (progn
;;       ;; (message "Did not detect embedded python")
;;       (elpy-enable)
;;       ;; it does ;; (add-hook 'python-mode-hook 'elpy-mode) so remove it
;;       (remove-hook 'python-mode-hook 'elpy-mode)
;;       (yapf-mode -1)
;;       (blacken-mode +1)
;;       ;; (py-autopep8-enable-on-save)
;;       ;; (make-variable-buffer-local 'elpy-modules)
;;       ;; (setq elpy-modules
;; 	  ;;         '(elpy-module-sane-defaults
;;       ;;           elpy-module-company
;;       ;;           elpy-module-eldoc
;;       ;;           elpy-module-flymake
;;       ;;           ;; elpy-module-folding
;;       ;;           elpy-module-highlight-indentation
;;       ;;           elpy-module-pyvenv
;;       ;;           elpy-module-yasnippet
;;       ;;           ))
;;       (elpy-mode +1))
;;     )
;;   )

;; ;;; https://github.com/syohex/emacs-company-jedi
;; ;; company-mode completion back-end for Python JEDI.
;; (use-package company-jedi
;;   :requires (company-mode)
;;   :init
;;   (add-to-list 'company-backends 'company-jedi))

;;; https://github.com/Wilfred/pip-requirements.el
;; major mode for editing pip requirements files.
;; Use its defaults
;; - already recognizes file names
;; - do not turn on the ac module as it will use company
(use-package pip-requirements)

;; ;; https://github.com/paetzke/py-autopep8.el
;; (use-package py-autopep8
;;   :config
;;   (setq py-autopep8-options '("--max-line-length=100"))
;;   ;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;   )

;; ;;; https://github.com/JorisE/yapfify
;; (use-package yapfify
;;   :delight yapf-mode)

;; ;; https://github.com/proofit404/blacken
;; (use-package blacken
;;   :config
;;   (setq
;;    ;; blacken-line-length                  'fill  ;; ‘fill-column’ variable value is used.
;;    ;; Only auto-blacken if project has a pyproject.toml with a [tool.black] section.
;;    blacken-only-if-project-is-blackened t
;;    blacken-skip-string-normalization    t))

;;; https://github.com/jorgenschaefer/pyvenv
(use-package pyvenv
  :init
  (setq pyvenv-default-virtual-env-name ".venv")
  ;; $WORKON_HOME or ~/.virtualenvs (Default)
  ;; (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyvenv-mode 1)
  :bind
  ("C-x C-y v" . pyvenv-activate))


;; ;;; https://github.com/pythonic-emacs/pyenv-mode
;; ;; macOS: brew install pyenv
;; ;; ubuntu: https://github.com/pyenv/pyenv-installer
;; (use-package pyenv-mode
;;   :if
;;   (executable-find "pyenv")
;;   :init
;;   (add-to-list 'exec-path "~/.pyenv/shims")
;;   (setenv "WORKON_HOME" "~/.pyenv/versions/")
;;   :config
;;   (pyenv-mode)
;;   :bind
;;   ("C-x C-y e" . pyenv-activate-current-project))

;; (defun pyenv-init()
;;   "Setup pyenv in Emacs."
;;   (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
;;   (message (concat "Setting pyenv version to " global-pyenv))
;;   (pyenv-mode-set global-pyenv)
;;   (defvar pyenv-current-version nil global-pyenv))

;; (defun pyenv-activate-current-project ()
;;   "Automatically activates pyenv version if .python-version file exists."
;;   (interactive)
;;   (f-traverse-upwards
;;    (lambda (path)
;;      (message path)
;;      (let ((pyenv-version-path (f-expand ".python-version" path)))
;;        (if (f-exists? pyenv-version-path)
;;           (progn
;;             (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
;;             (pyenv-mode-set pyenv-current-version)
;;             (pyvenv-workon pyenv-current-version)
;;             (message (concat "Setting virtualenv to " pyenv-current-version))))))))

;; (add-hook 'after-init-hook 'pyenv-init)
;; (add-hook 'projectile-after-switch-project-hook #'pyenv-activate-current-project)

;;----------------------------------------------------------------------------
;; Cython
;;----------------------------------------------------------------------------

;; ;; pip install Cython
;; (use-package cython-mode)
;; (use-package flycheck-cython)
;; (add-hook 'cython-mode-hook 'flycheck-mode)

(provide 'lang-python)

;;; lang-python.el ends here
