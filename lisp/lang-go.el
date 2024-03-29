;;; lang-go.el --- golang integration

;;; Commentary:

;; SEE ALSO: flycheck-gometalinter, flycheck-golangci-lint

;;; Code:


(use-package go-mode
  :config
  ; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
  (add-hook 'go-mode-hook 'company-mode)
  ;; Call Gofmt before saving
  ;;(add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook
            #'(lambda ()
               (add-hook 'before-save-hook 'gofmt-before-save nil
                         ;; Buffer local hook.
                         t)))
  (add-hook 'go-mode-hook 'setup-go-mode-compile)
  (add-hook 'go-mode-hook #'smartparens-mode)
  (add-hook 'go-mode-hook #'(lambda ()
			     (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
  (add-hook 'go-mode-hook #'(lambda ()
			     (local-set-key (kbd "C-c C-g") 'go-goto-imports)))
  (add-hook 'go-mode-hook (lambda ()
			    (set (make-local-variable 'company-backends) '(company-go))
			    (company-mode))))

(use-package company-go
  :after go-mode
  :after company
  :config
  (setq tab-width 4)
  :bind (:map go-mode-map
              ;; Godef jump key binding
              ("M-." . godef-jump)))

(use-package go-eldoc
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(defun setup-go-mode-compile ()
  "Customize compile command to run go build."
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(provide 'lang-go)

;;; lang-go.el ends here
