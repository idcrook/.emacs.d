;;; lang-web.el --- Other web-related language integration

;;; Commentary:

;; (use-package add-node-modules-path) ;; looks in node_modules/bin

;; Additional possibilities (See also: lang-javascript.el)
;;
;; https://github.com/jtkDvlp/web-mode-edit-element
;; https://github.com/russmatney/company-css-classes
;; https://github.com/felipeochoa/rjsx-mode

;;; Code:

(use-package web-mode
  :after flycheck
  ;; :bind (("C-c ]" . emmet-next-edit-point)
  ;;        ("C-c [" . emmet-prev-edit-point)
  ;;        ("C-c o b" . browse-url-of-file))
  :mode
  (("\\.html?\\'" . web-mode)
   ("\\.s?css\\'" . web-mode)
   ;; ("\\.js\\'" . web-mode)
   ;; ("\\.phtml?\\'" . web-mode)
   ;; ("\\.tpl\\.php\\'" . web-mode)
   ;; ("\\.[agj]sp\\'" . web-mode)
   ;; ("\\.as[cp]x\\'" . web-mode)
   ;; ("\\.erb\\'" . web-mode)
   ;; ("\\.mustache\\'" . web-mode)
   ;; ("\\.djhtml\\'" . web-mode)
      ("\\.jsx$" . web-mode))
  :config

  ;; highlight enclosing tags of the element under cursor
  (setq web-mode-enable-current-element-highlight t)

  ;; snippets for HTML
  ;; https://github.com/smihica/emmet-mode
  (use-package emmet-mode
    :init (setq emmet-move-cursor-between-quotes t) ;; default nil
    :diminish (emmet-mode . "m"))
  (add-hook 'web-mode-hook  'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  ;; To enable the JSX support, add your major-mode to emmet-jsx-major-modes

  (defun my-web-mode-hook ()
    "Hooks for `web-mode'."
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-code-indent-offset 2))
  (add-hook 'web-mode-hook  'my-web-mode-hook)

  ;;;; https://github.com/osv/company-web
  ;; to get completion for HTML stuff
  (use-package company-web
    :init
    ;; you may key bind, for example for web-mode:
    (define-key web-mode-map (kbd "C-'") 'company-web-html)
    (define-key web-mode-map (kbd "C-\"") 'company-complete)
    ;; pollutes M-SPC ;; (define-key web-mode-map (kbd "M-SPC") 'company-complete)
    )

  ;; (defun my-company-web-mode-hook ()
  ;;   "Hook for `web-mode' config for company-backends."
  ;;   (set (make-local-variable 'company-backends)
  ;;        '((company-capf company-web-html company-files))))  ;; company-yasnippet
  ;; (add-hook 'web-mode-hook 'my-company-web-mode-hook)

  ;; (add-hook 'web-mode-hook 'company-mode)
  )

;; some vestigial web-mode related commmented out here

;; ;; editing enhancements for web-mode
  ;; ;; https://github.com/jtkDvlp/web-mode-edit-element
  ;; (use-package web-mode-edit-element
  ;;   :diminish web-mode-edit-element-minor-mode
  ;;   :config (add-hook 'web-mode-hook 'web-mode-edit-element-minor-mode))
    ;; (add-hook 'web-mode-hook 'jsx-flycheck)

  ;; ;; https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking
  ;; (defadvice web-mode-highlight-part (around tweak-jsx activate)
  ;;   (if (equal web-mode-content-type "jsx")
  ;;       (let ((web-mode-enable-part-face nil))
  ;;         ad-do-it)
  ;;     ad-do-it))

  ;; jsxhint is deprecated and broken 2019-Jun-15
  ;; ReferenceError: primordials is not defined
  ;; ;; npm install --global gulp-cli
  ;; ;; npm install -g jsxhint
  ;; (flycheck-define-checker jsxhint-checker
  ;;   "A JSX syntax and style checker based on JSXHint."

  ;;   :command ("jsxhint" "--harmony" source)
  ;;   :error-patterns
  ;;   ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  ;;   :modes (web-mode))

  ;; (defun jsx-flycheck ()
  ;;   (when (equal web-mode-content-type "jsx")
  ;;     ;; enable flycheck
  ;;     (flycheck-select-checker 'jsxhint-checker)
  ;;     (flycheck-mode)))

;; ;;; for vue.js / .vue files - https://github.com/AdamNiederer/vue-mode
;; (use-package vue-mode)

;; (use-package rjsx-mode
;;   :mode "\\.jsx$"
;;   :mode "components/.+\\.js$"
;;   :init
;;   ;; auto-detect JSX file
;;   ;; https://github.com/shahinism/emacs.d/blob/master/configs.org
;;   ;; source: https://github.com/hlissner/.emacs.d/blob/master/modules/lang/javascript/config.el
;;   (push (cons (lambda ()
;;                 (and buffer-file-name
;;                      (equal (file-name-extension buffer-file-name) "js")
;;                      (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
;;                                         magic-mode-regexp-match-limit t)
;;                      (progn
;;                        (goto-char (match-beginning 1))
;;                        ;; sp-point-in-string-or-comment requires smartparens package
;;                        (not (sp-point-in-string-or-comment)))))
;;               'rjsx-mode)
;;         magic-mode-alist)
;;   :config
;;   ;; match gatsby.js layout
;;   (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
;;   (add-to-list 'auto-mode-alist '("layouts\\/.*\\.js\\'" . rjsx-mode))
;;   (add-to-list 'auto-mode-alist '("templates\\/.*\\.js\\'" . rjsx-mode))
;;   (add-to-list 'auto-mode-alist '("pages\\/.*\\.js\\'" . rjsx-mode)))

;; configure CSS mode company backends
(use-package css-mode
  :config
  (defun my-css-mode-hook ()
    (set (make-local-variable 'company-backends)
         ;; '((company-css company-dabbrev-code company-files))))
  ;; In Emacs 26 and newer, company-css is removed from company-backends. company-capf is used instead.
         '((company-capf company-dabbrev-code company-files))))
  (add-hook 'css-mode-hook 'my-css-mode-hook)
  (add-hook 'css-mode-hook 'company-mode))


;;; https://github.com/yasuyk/web-beautify
;; ;; npm -g install js-beautify
;; (use-package web-beautify)

;;----------------------------------------------------------------------------
;; graphql
;;----------------------------------------------------------------------------
;; ;;; https://github.com/davazp/graphql-mode
;; (use-package graphql-mode)


;; ;; impatient mode - Live refresh of web pages
;; ;; https://github.com/skeeto/impatient-mode
;; (use-package impatient-mode
;;   :diminish (impatient-mode . " i")
;;   :commands (impatient-mode))

(provide 'lang-web)

;;; lang-web.el ends here
