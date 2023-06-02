;;; base-lsp.el --- Language Server Protocol integration

;;; Commentary:

;;; Code:

;;;________________________________________________________________________
;; Language Server Project (LSP) stuff

;; ;;; reference: https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/

;;; https://emacs-lsp.github.io/lsp-mode/page/performance/#increase-the-amount-of-data-which-emacs-reads-from-the-process
(setq read-process-output-max (* 1024 1024)) ;; 1mb


;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-c e")
;;(setq lsp-keymap-prefix "C-c l")

;;; https://github.com/emacs-lsp/lsp-mode
;;; https://emacs-lsp.github.io/lsp-mode/page/installation/
(use-package lsp-mode
  :config
  (define-key lsp-mode-map (kbd "C-c e") lsp-command-map)
  ;; (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  ;;:bind-keymap ("C-c l" . lsp-command-map)
  :hook (
;;         (python-mode . lsp)
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


;;; https://github.com/fredcamps/lsp-jedi
;; pip install -U jedi-language-server
(use-package lsp-jedi)


;; - Python (Microsoft)
;;; https://emacs-lsp.github.io/lsp-python-ms/
;; (use-package lsp-python-ms
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                           (require 'lsp-python-ms)
;;                           (lsp))))  ; or lsp-deferred

;; - OpenSCAD
;;; https://emacs-lsp.github.io/lsp-mode/page/lsp-openscad/
;; cargo install openscad-lsp

;; - TOML
;;; https://emacs-lsp.github.io/lsp-mode/page/lsp-toml/
;; cargo install taplo-cli --features lsp


;;; https://github.com/fredcamps/lsp-jedi
;;;

(provide 'base-lsp)

;;; base-lsp.el ends here
