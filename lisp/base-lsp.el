;;; base-lsp.el --- Language Server Protocol integration

;;; Commentary:

;;; Code:

;;;________________________________________________________________________
;; Language Server Project (LSP) stuff

;;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c e")

;;; https://github.com/emacs-lsp/lsp-mode
;;; https://emacs-lsp.github.io/lsp-mode/page/installation/
(use-package lsp-mode
  ;;; https://robert.kra.hn/posts/rust-emacs-setup/
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (define-key lsp-mode-map (kbd "C-c e") lsp-command-map)

  ;;; https://emacs-lsp.github.io/lsp-mode/page/remote/
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))

  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :hook (
;;        (python-mode . lsp)
        (python-mode . lsp-deferred)
         ;; if you want which-key integration - see base-extensions.el for whick-key package
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp)

;;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;;; https://emacs-lsp.github.io/dap-mode/page/configuration/#python
;; pip3 install --user "debugpy"

;; (use-package dap-mode
;;   :after lsp-mode
;;   :commands dap-debug
;;   :hook ((python-mode . dap-ui-mode) (python-mode . dap-mode))
;;   :config
;;   (require 'dap-python)
;;   (setq dap-python-debugger 'debugpy)
;;   (defun dap-python--pyenv-executable-find (command)
;;     (with-venv (executable-find "python3")))

;;   (add-hook 'dap-stopped-hook
;;             (lambda (arg) (call-interactively #'dap-hydra))))

;; Python ;;
;;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;; pip install 'python-lsp-server[all]'

;;; [astral-sh/ruff-lsp: A Language Server Protocol implementation for Ruff.](https://github.com/astral-sh/ruff-lsp)


;; Python ;; Dead ends
;;; https://github.com/fredcamps/lsp-jedi
;;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pyls/
;; pip install --user "python-language-server[all]"
;;; https://github.com/fredcamps/lsp-jedi
;; pip install -U jedi-language-server
;;(use-package lsp-jedi)
;;; https://emacs-lsp.github.io/lsp-python-ms/
;; - Python (Microsoft)
;; (use-package lsp-python-ms
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

;; OpenSCAD ;;
;;; https://emacs-lsp.github.io/lsp-mode/page/lsp-openscad/
;; cargo install openscad-lsp

;; TOML ;;
;;; https://emacs-lsp.github.io/lsp-mode/page/lsp-toml/
;; cargo install taplo-cli --features lsp

(provide 'base-lsp)

;;; base-lsp.el ends here
