;;; lang-elixir.el --- elixir integration

;;; Commentary:

;;; Code:

(use-package alchemist
  :config
  (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package flycheck-elixir
  :config
  (add-hook 'elixir-mode-hook 'flycheck-mode))

;; (use-package flycheck-mix
;;   :commands (flycheck-mix-setup))

(use-package elixir-mode)

(provide 'lang-elixir)

;;; lang-elixir.el ends here
