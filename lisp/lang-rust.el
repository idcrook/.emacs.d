;;; lang-rust.el --- rust integration

;;; Commentary:

;;; Code:

;; rust-mode, cargo, rust-analyzer (LSP)

;; investigate polymode for editting doc comments

;; macOS rust installation
;;     brew install rustup-init
;;     rustup-init  # accept defaults
;;     # (revert dotfile edits)

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :config
  (progn
    ;; add flycheck support for rust (reads in cargo stuff)
    ;; https://github.com/flycheck/flycheck-rust
    (use-package flycheck-rust)

    ;; cargo-mode for all the cargo related operations
    ;; https://github.com/kwrooijen/cargo.el
    (use-package cargo
      :hook (rust-mode . cargo-minor-mode)
      :bind
      ("C-c C-c C-n" . cargo-process-new)) ;; global binding

    ;;; separedit ;; via https://github.com/twlz0ne/separedit.el
    (use-package separedit
      :straight (separedit :type git :host github :repo "idcrook/separedit.el")
      :config
      (progn
        (define-key prog-mode-map (kbd "C-c '") #'separedit)
        (setq separedit-default-mode 'markdown-mode)))

    (add-hook 'rust-mode-hook 'flycheck-mode)

    ;;; https://rust-analyzer.github.io/manual.html#emacs
    ;;; https://rust-analyzer.github.io/manual.html#installation
    ;; rustup component add rust-src
    (add-hook 'rust-mode-hook 'lsp-deferred)
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

    ;; format rust buffers on save using rustfmt
    (add-hook 'before-save-hook
              (lambda ()
                (when (eq major-mode 'rust-mode)
                  (rust-format-buffer))))))

(provide 'lang-rust)

;;; lang-rust.el ends here
