;;; base-lsp.el --- Language Server Protocol integration

;;; Commentary:

;;; Code:

;;;________________________________________________________________________
;; Language Server Project (LSP) stuff

;; ;;; reference: https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/

;;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands lsp)

;;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; (use-package company-lsp
;;   :commands company-lsp
;;   :config
;;   (push 'company-lsp company-backends))

;;; emacs lsp-mode client for Microsoft's python language server
;; https://github.com/andrew-christianson/lsp-python-ms

;; (use-package lsp-python-ms
;;   :straight (:host github :repo "andrew-christianson/lsp-python-ms")

;;   :hook (python-mode . lsp)
;;   :config

;;   ;; for dev build of language server
;;   (setq lsp-python-ms-dir
;;         ;; (expand-file-name "~/python-language-server/output/bin/Release/"))
;;         (expand-file-name "~/.emacs.d/site-lisp/github/python-language-server/output/bin/Release/"))
;;   ;; for executable of language server, if it's not symlinked on your PATH
;;   ;; (setq lsp-python-ms-executable
;;   ;;       "~/python-language-server/output/bin/Release/osx-x64/publish/Microsoft.Python.LanguageServer"))
;;   ;; symlinked to ~/bin/macos/Microsoft.Python.LanguageServer
;;   )


(provide 'base-lsp)

;;; base-lsp.el ends here
