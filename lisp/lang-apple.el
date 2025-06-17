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

;;;________________________________________________________________________
;; Swift and ObjC
;;
;; https://www.swift.org/documentation/articles/zero-to-swift-emacs.html

;;; Locate sourcekit-lsp
(defun find-sourcekit-lsp ()
  (or (executable-find "sourcekit-lsp")
      (and (eq system-type 'darwin)
           (string-trim (shell-command-to-string "xcrun -f sourcekit-lsp")))
      "/usr/local/swift/usr/bin/sourcekit-lsp"))


;; Swift editing support
(use-package swift-mode
  :mode "\\.swift\\'"
  :interpreter "swift"
  ;; :config
  ;; (setq swift-mode:basic-offset 2)
  )

;; ;;; https://github.com/swift-emacs/swift-mode
;; (use-package swift-mode
;;   ;; enable lsp automatically when a .swift file is visited
;;   :hook (swift-mode . (lambda () (lsp)))
;;   :config
;;   (setq swift-mode:basic-offset 2))

;;; https://github.com/emacs-lsp/lsp-sourcekit
(use-package lsp-sourcekit
  :after lsp-mode
  ;;:config
 :custom
 (lsp-sourcekit-executable (find-sourcekit-lsp) "Find sourcekit-lsp"))

;; (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))
;; xcrun --find sourcekit-lsp


;;;________________________________________________________________________
;; Applescript

;; ;;; https://github.com/stig/ob-applescript.el
;; (use-package ob-applescript)

;; ;; https://github.com/tequilasunset/apples-mode
;; (use-package apples-mode)

;;;________________________________________________________________________
;; Other macOS related
;; in general should be safe to include in Linux platforms

;;; https://github.com/xuchunyang/grab-mac-link.el
(use-package grab-mac-link
  :config
  (setq grab-mac-link-dwim-favourite-app 'safari))

;; commenting out until resolution of:
;;     https://github.com/raghavgautam/osx-lib/issues/12
;; ;;; https://github.com/raghavgautam/osx-lib
;; (use-package osx-lib)

;; Provides functions for:
;;   1. Running Apple Script / osascript
;;   2. Play beep
;;      (setq ring-bell-function #'osx-lib-do-beep)
;;   3. Notification functions
;;      (osx-lib-notify2 "Emacs" "Text Editor")
;;   4. Copying to/from clipboard
;;   5. Show the current file in Finder.  Works with dired.
;;      (osx-lib-reveal-in-finder)
;;   6. Get/Set Sound volume
;;      (osx-lib-set-volume 25)
;;      (osx-lib-get-volume)
;;   6. Mute/unmute Sound volume
;;      (osx-lib-mute-volume)
;;      (osx-lib-unmute-volume)
;;   8. VPN Connect/Disconnect
;;      (defun work-vpn-connect ()
;;        "Connect to Work VPN."
;;        (interactive)
;;        (osx-lib-vpn-connect "WorkVPN" "VPN_Password"))
;;   9. Use speech
;;      (osx-lib-say "Emacs")
;;   10.Use mdfind(commandline equivalent of Spotlight) for locate
;;      (setq locate-make-command-line #'osx-locate-make-command-line)
;;

;; ;;; https://github.com/kaz-yos/reveal-in-osx-finder
;; ;; can be handled by osx-lib-reveal-in-finder
;; (use-package reveal-in-osx-finder
;;   :bind (("C-c z" . reveal-in-osx-finder)))

;; ;; system search with spotlight
;; (use-package spotlight
;;   :config
;;   (setq spotlight-tmp-file
;; 	(expand-file-name "spotlight-tmp-file" dpc/temp-dir)))

(provide 'lang-apple)

;;; lang-apple.el ends here
