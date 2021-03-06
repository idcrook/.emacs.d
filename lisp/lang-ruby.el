;;; lang-ruby.el --- ruby integration

;;; Commentary:

;;; Code:

;; (use-package enh-ruby-mode
;;   :mode
;;   (("\\.rb\\'" . ruby-mode)))

;; ;;; https://github.com/dgutov/robe
;; ;; M-x robe-start
;; (use-package robe
;;   :defer 2
;;   :init
;;   (eval-after-load 'company
;;     '(push 'company-robe company-backends))
;;   :config
;;   (add-hook 'ruby-mode-hook 'robe-mode))

;; (use-package rinari)

(provide 'lang-ruby)

;;; lang-ruby.el ends here
