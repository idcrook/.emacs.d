;;; lang-apple.el --- Apple platforms and languages integration

;;; Commentary:

;;; Code:


;;;________________________________________________________________________
;; Swift and ObjC
;;

;; Investigate
;; https://github.com/danielmartin/swift-helpful
;; https://github.com/GyazSquare/flycheck-objc-clang
;; https://gitlab.com/michael.sanders/swift-playground-mode


;;; https://github.com/emacs-lsp/lsp-sourcekit
;; FIXME: handle Linux platform and/or other platform considerations
;; Problems:
;; - 2020-07-13 : sourcekit-lsp does not find XCTest
(use-package lsp-sourcekit
  :after lsp-mode
  :config
  ;; xcrun --find sourcekit-lsp
  ;;(setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
  (setq lsp-sourcekit-executable "/Applications/Xcode-beta.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp")
  )

;;; https://github.com/swift-emacs/swift-mode
(use-package swift-mode
  ;; enable lsp automatically when a .swift file is visited
  :hook (swift-mode . (lambda () (lsp)))
  :config
  (setq swift-mode:basic-offset 4))

;;;________________________________________________________________________
;; Applescript

;; ;;; https://github.com/stig/ob-applescript.el
;; (use-package ob-applescript)

;; https://github.com/tequilasunset/apples-mode
(use-package apples-mode)

;;;________________________________________________________________________
;; Other macOS related
;; in general should be safe to include in Linux platforms

;; https://github.com/xuchunyang/grab-mac-link.el
(use-package grab-mac-link
  :config
  (setq grab-mac-link-dwim-favourite-app 'safari)
  )

;; https://github.com/raghavgautam/osx-lib
(use-package osx-lib)

;; ;;; https://github.com/kaz-yos/reveal-in-osx-finder
;; ;; can be handled by osx-lib-revel-in-finder
;; (use-package reveal-in-osx-finder
;;   :bind (("C-c z" . reveal-in-osx-finder)))

;; ;; system search with spotlight
;; (use-package spotlight
;;   :config
;;   (setq spotlight-tmp-file
;; 	(expand-file-name "spotlight-tmp-file" temp-dir)))

(provide 'lang-apple)

;;; lang-apple.el ends here
