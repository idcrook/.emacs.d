;;; package --- python configs
;;; Commentary:
;;; Contains my python configs
;; via http://emacs-bootstrap.com

;;; Code:

;;; elpy : https://github.com/jorgenschaefer/elpy
;; # completion and code navigation - jedi
;; # automatic formatting - black autopep8 yapf
;; # code checks - flake8
;; pip3 install --user jedi flake8 black autopep8 yapf
;; python3 -m pip install --user --upgrade jedi flake8 black autopep8 yapf

(use-package python
  :mode ("\\.py$" . python-mode)
  :config
  (setq python-shell-interpreter "python3"
        python-shell-interpreter-args "-i")
  (use-package elpy
    :after flycheck
    :delight highlight-indentation-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
    :config
    (setq elpy-rpc-backend "jedi")
    (setq elpy-rpc-python-command "python3")

    :bind (:map elpy-mode-map
	      ("M-." . elpy-goto-definition)
	      ("M-," . pop-tag-mark)))
  (elpy-enable))


;;; https://github.com/syohex/emacs-company-jedi
;; company-mode completion back-end for Python JEDI.
(use-package company-jedi
  :requires (company-mode)
  :config
  (add-to-list 'company-backends 'company-jedi))

;; major mode for editing pip requirements files.
(use-package pip-requirements
  :mode ("/requirements.txt$" . pip-requirements-mode)
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

;; ;; https://github.com/paetzke/py-autopep8.el
;; (use-package py-autopep8
;;   :config
;;   (setq py-autopep8-options '("--max-line-length=100"))
;;   ;;(add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
;;   )

;; https://github.com/JorisE/yapfify
(use-package yapfify
  :init
  (add-hook 'python-mode-hook 'yapf-mode))

;; ;; https://github.com/proofit404/blacken
;; (use-package blacken
;;   :init
;;   (add-hook 'python-mode-hook 'blacken-mode))

;; macOS: brew install pyenv
;; ubuntu: https://github.com/pyenv/pyenv-installer
(use-package pyenv-mode
  :if
  (executable-find "pyenv")
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-init()
  "Setup pyenv in Emacs."
  (setq global-pyenv (replace-regexp-in-string "\n" "" (shell-command-to-string "pyenv global")))
  (message (concat "Setting pyenv version to " global-pyenv))
  (pyenv-mode-set global-pyenv)
  (defvar pyenv-current-version nil global-pyenv))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (f-traverse-upwards
   (lambda (path)
     (message path)
     (let ((pyenv-version-path (f-expand ".python-version" path)))
       (if (f-exists? pyenv-version-path)
          (progn
            (setq pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8)))
            (pyenv-mode-set pyenv-current-version)
            (pyvenv-workon pyenv-current-version)
            (message (concat "Setting virtualenv to " pyenv-current-version))))))))

(add-hook 'after-init-hook 'pyenv-init)
(add-hook 'projectile-after-switch-project-hook #'pyenv-activate-current-project)

;;; emacs lsp-mode client for Microsoft's python language server
;; https://github.com/andrew-christianson/lsp-python-ms

(use-package lsp-python-ms
  :straight (:host github :repo "andrew-christianson/lsp-python-ms")

  :hook (python-mode . lsp)
  :config

  ;; for dev build of language server
  (setq lsp-python-ms-dir
        ;; (expand-file-name "~/python-language-server/output/bin/Release/"))
        (expand-file-name "~/.emacs.d/user_elisp/github/python-language-server/output/bin/Release/"))
  ;; for executable of language server, if it's not symlinked on your PATH
  ;; (setq lsp-python-ms-executable
  ;;       "~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))
  ;; symlinked to ~/bin/macos/Microsoft.Python.LanguageServer
  )

;;; Cython
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; pip install Cython

(use-package cython-mode)

(use-package flycheck-cython)
(add-hook 'cython-mode-hook 'flycheck-mode)

(provide 'lang-python)

;;; lang-python.el ends here
