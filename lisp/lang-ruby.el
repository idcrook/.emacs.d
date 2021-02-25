;;; lang-ruby.el --- elixir integration

;;; Commentary:

;;; Code:

(use-package enh-ruby-mode
  :mode
  (("\\.rb\\'" . ruby-mode)))

;; https://github.com/dgutov/robe
;; M-x robe-start
(use-package robe
  :config
  (add-hook 'ruby-mode-hook 'robe-mode))

(eval-after-load 'company
  '(push 'company-robe company-backends))


(use-package rinari)

(provide 'lang-ruby)

;;; lang-ruby.el ends here
