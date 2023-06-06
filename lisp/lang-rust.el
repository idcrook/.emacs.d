;;; lang-rust.el --- rust integration

;;; Commentary:

;;; Code:

;; rust-mode, cargo, rust-analyzer (LSP)

;; investigate polymode for editting doc comments

;; macOS rust installation
;;     brew install rustup-init
;;     rustup-init  # accept defaults
;;     # (revert dotfile edits)


;; rust-analyzer: one of
;;     rustup component add rust-src
;;     brew install rust-analyzer


;; ;; rust-mode
;; ;; https://github.com/rust-lang/rust-mode
;; (use-package rust-mode
;;   :config
;;   (progn
;;     ;; add flycheck support for rust (reads in cargo stuff)
;;     ;; https://github.com/flycheck/flycheck-rust
;;     (use-package flycheck-rust)

;;     ;; cargo-mode for all the cargo related operations
;;     ;; https://github.com/kwrooijen/cargo.el
;;     (use-package cargo
;;       :hook (rust-mode . cargo-minor-mode)
;;       :bind
;;       ("C-c C-c C-n" . cargo-process-new)) ;; global binding

;;     ;;; separedit ;; via https://github.com/twlz0ne/separedit.el
;;     (use-package separedit
;;       :straight (separedit :type git :host github :repo "idcrook/separedit.el")
;;       :config
;;       (progn
;;         (define-key prog-mode-map (kbd "C-c '") #'separedit)
;;         (setq separedit-default-mode 'markdown-mode)))

;;     (add-hook 'rust-mode-hook 'flycheck-mode)

;;     ;;; https://rust-analyzer.github.io/manual.html#emacs
;;     ;;; https://rust-analyzer.github.io/manual.html#installation
;;     ;; rustup component add rust-src
;;     (add-hook 'rust-mode-hook 'lsp-deferred)
;;     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

;;     ;; format rust buffers on save using rustfmt
;;     (add-hook 'before-save-hook
;;               (lambda ()
;;                 (when (eq major-mode 'rust-mode)
;;                   (rust-format-buffer))))))

;;; https://github.com/brotzeit/rustic
;; https://robert.kra.hn/posts/rust-emacs-setup/
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

(provide 'lang-rust)

;;; lang-rust.el ends here
