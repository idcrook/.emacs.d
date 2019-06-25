;;; lang-javascript.el --- Javascriptish language integration

;;; Commentary:

;;; Code:


;; to install it as a minor mode just for JavaScript linting,
(if (version< emacs-version "27.0")
    (message "is before emacs 27.0")
    (add-hook 'js-mode-hook 'js2-minor-mode))


;; https://github.com/mooz/js2-mode
(use-package js2-mode
  :bind (:map js2-mode-map
              (("C-x C-e" . js-send-last-sexp)
               ("C-M-x" . js-send-last-sexp-and-go)
               ("C-c C-b" . js-send-buffer-and-go)
               ("C-c C-l" . js-load-file-and-go)))
  ;; :mode
  ;; ("\\.js$" . js2-mode)
  ;; ("\\.json$" . js2-jsx-mode)
  :config
  ;; (custom-set-variables '(js2-strict-inconsistent-return-warning nil))
  ;; (custom-set-variables '(js2-strict-missing-semi-warning nil))
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-strict-missing-semi-warning nil)

  (setq js-indent-level 2)
  (setq js2-basic-offset 2)

  ;; tern :- IDE like features for javascript and completion
  ;; http://ternjs.net/doc/manual.html#emacs

  ;;tern-django -  Create tern projects for django applications.

  ;; on macOS
  ;;   npm install tern -g
  ;; on Linux (Ubuntu)
  ;;   sudo npm install tern -g
  (use-package tern
    :config
    (defun my-js-mode-hook ()
      "Hook for `js-mode'."
      (set (make-local-variable 'company-backends)
           '((company-tern company-files))))

    ;; finish js2 config
    (add-hook 'js2-mode-hook 'my-js-mode-hook)
    (add-hook 'js2-mode-hook 'company-mode)
    (add-hook 'js2-mode-hook 'tern-mode))

  ;; Enable JavaScript completion between <script>...</script> etc.
  (defadvice company-tern (before web-mode-set-up-ac-sources activate)
    "Set `tern-mode' based on current language before running company-tern."
    (message "advice")
    (if (equal major-mode 'web-mode)
	(let ((web-mode-cur-language
	       (web-mode-language-at-pos)))
	  (if (or (string= web-mode-cur-language "javascript")
		  (string= web-mode-cur-language "jsx"))
	      (unless tern-mode (tern-mode))
	    (if tern-mode (tern-mode -1))))))

  ;; company backend for tern
  ;; http://ternjs.net/doc/manual.html#emacs
  (use-package company-tern)

  ;; Run a JavaScript interpreter in an inferior process window
  ;; https://github.com/redguardtoo/js-comint
  (use-package js-comint)

  ;; js2-refactor :- refactoring options for emacs
  ;; https://github.com/magnars/js2-refactor.el
  (use-package js2-refactor
    :diminish js2-refactor-mode
    :config
    (js2r-add-keybindings-with-prefix "C-c j r"))
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

;; ;;; https://github.com/ananthakumaran/tide - TypeScript support
;; (use-package tide)

;;ts-comint - TypeScript  Run a Typescript interpreter in an inferior process window.

;; (defun setup-tide-mode ()
;;   "Configure TypeScript support."
;;   (interactive)
;;   (tide-setup)
;;   (flycheck-mode +1)
;;   (setq flycheck-check-syntax-automatically '(save mode-enabled))
;;   (eldoc-mode +1)
;;   (tide-hl-identifier-mode +1)
;;   (company-mode +1))

;; ;; formats the buffer before saving
;; (add-hook 'before-save-hook 'tide-format-before-save)

;; (add-hook 'typescript-mode-hook #'setup-tide-mode)



(provide 'lang-javascript)

;;; lang-javascript.el ends here
