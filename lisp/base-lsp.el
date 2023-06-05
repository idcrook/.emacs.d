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
  :config
  (define-key lsp-mode-map (kbd "C-c e") lsp-command-map)

  ;;; https://emacs-lsp.github.io/lsp-mode/page/remote/
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                    :major-modes '(python-mode)
                    :remote? t
                    :server-id 'pyls-remote))
  :hook (
;;        (python-mode . lsp)
        (python-mode . lsp-deferred)
         ;; if you want which-key integration - see base-extensions.el for whick-key package
         (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp)

;;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)

;; if you are ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; Python ;;
;;; https://emacs-lsp.github.io/lsp-mode/page/lsp-pylsp/
;; pip install 'python-lsp-server[all]'

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
