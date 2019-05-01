;;; lang-apple.el --- Apple platforms and languages integration

;;; Commentary:

;;; Code:


;;; https://github.com/stig/ob-applescript.el
(use-package ob-applescript)

;; https://github.com/tequilasunset/apples-mode
(use-package apples-mode)

;; ;; https://github.com/Lindydancer/objc-font-lock
;; (use-package objc-font-lock-mode
;;   :config (objc-font-lock-global-mode 1)
;;   )

;; https://github.com/GyazSquare/flycheck-objc-clang
(use-package flycheck-objc-clang)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-objc-clang-setup))

;; (custom-set-variables
;;  '(flycheck-objc-clang-modules t) ; The modules feature is disabled by default
;;  '(flycheck-objc-clang-arc t))    ; The objc arc feature is disabled by default

;; https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  :config
  (setq swift-mode:basic-offset 4))

;
;; https://github.com/GyazSquare/flycheck-swift3
(use-package flycheck-swift3)
(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-swift3-setup))

;; ;; https://github.com/jojojames/flycheck-swiftlint
;; (use-package flycheck-swiftlint)
;; (with-eval-after-load 'flycheck
;;   (flycheck-swiftlint-setup))

;; ;; https://github.com/nathankot/company-sourcekit
;; ;; requires https://github.com/terhechte/SourceKittenDaemon
;; (use-package company-sourcekit
;;   :config
;;   (add-to-list 'company-backends 'company-sourcekit))

;; https://github.com/zweifisch/ob-swift
(use-package ob-swift)

;; https://github.com/jojojames/flycheck-xcode
(use-package flycheck-xcode
  :commands (flycheck-xcode-setup)
  :init
  (mapc
   (lambda (x)
     (add-hook x #'flycheck-xcode-setup))
   '(c-mode-hook c++-mode-hook objc-mode-hook swift-mode-hook)))

;;;________________________________________________________________________
;; Other macOS related
;; should be safe to include in the Linux installs

;; https://github.com/xuchunyang/grab-mac-link.el
(use-package grab-mac-link)

;; https://github.com/raghavgautam/osx-lib
(use-package osx-lib)

(use-package reveal-in-osx-finder
  :bind (("C-c z" . reveal-in-osx-finder)))

;; system search with spotlight
(use-package spotlight
  :config
  (setq spotlight-tmp-file
	(expand-file-name "spotlight-tmp-file" temp-dir)))

(provide 'lang-apple)

;;; lang-apple.el ends here
