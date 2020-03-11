;;; lang-rust.el --- rust integration

;;; Commentary:

;;; Code:

;; start from http://emacs-bootstrap.com/

;; rust-mode, racer, cargo

;; macOS rust installation
;;     brew install rustup-init
;;     rustup-init  # accept defaults
;;     # (revert dotfile edits)

;; rust-mode
;; https://github.com/rust-lang/rust-mode
(use-package rust-mode
  :bind ( :map rust-mode-map
               (("C-c C-t" . racer-describe)
                ([?\t] .  company-indent-or-complete-common)))
      ;;(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
      ;; :bind-keymap
      ;; ([?\t] .  company-indent-or-complete-common)
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
      ("C-c C-c C-n" . cargo-process-new))

    ;; TODO: investigate LSP
    ;; https://www.reddit.com/r/rust/comments/au3z5j/emacs_lately/
    ;; rls
    ;;
    ;; error: 'rls' is not installed for the toolchain
    ;; 'stable-x86_64-unknown-linux-gnu'
    ;; To install, run `rustup component add rls --toolchain
    ;; stable-x86_64-unknown-linux-gnu`


    ;;; racer-mode for getting IDE like features for rust-mode
    ;; https://github.com/racer-rust/emacs-racer
    ;;
    ;; ;; https://github.com/racer-rust/emacs-racer#installation
    ;; rustup toolchain add nightly
    ;; rustup component add rust-src
    ;; cargo +nightly install racer
    ;;
    (use-package racer
      :hook (rust-mode . racer-mode)
      :config
      (progn
        ;; package does this by default ;; set racer rust source path environment variable
        ;; (setq racer-rust-src-path (getenv "RUST_SRC_PATH"))
        (defun my-racer-mode-hook ()
          (set (make-local-variable 'company-backends)
               '((company-capf company-files)))
          (setq company-minimum-prefix-length 1))

        (add-hook 'racer-mode-hook 'my-racer-mode-hook)

        ;; enable company and eldoc minor modes in rust-mode (racer-mode)
        (add-hook 'racer-mode-hook #'company-mode)
        (add-hook 'racer-mode-hook #'eldoc-mode)))

    (add-hook 'rust-mode-hook 'flycheck-mode)
    (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

    ;; format rust buffers on save using rustfmt
    (add-hook 'before-save-hook
              (lambda ()
                (when (eq major-mode 'rust-mode)
                  (rust-format-buffer))))))

(provide 'lang-rust)

;;; lang-rust.el ends here
