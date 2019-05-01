;;; lang-cpp.el --- C++ and C language dialects integration

;;; Commentary:

;; SEE ALSO: lang-apple.el

;;; Code:

;; https://github.com/stardiviner/arduino-mode
(use-package arduino-mode)

;; https://github.com/yuutayamada/company-arduino
;; (use-package company-arduino)  ;; requires:  irony-mode, company-irony and company-c-headers
;; Note
;; This package's default configuration is set for Linux environment, which I'm currently using, so if you are using different environment (Mac or Windows), please change the Arduino's directory paths accordingly. Related variables: company-arduino-sketch-directory-regex, company-arduino-home, company-arduino-header, company-arduino-includes-dirs and irony-arduino-includes-options.

;; ;; https://github.com/randomphrase/company-c-headers
;; (use-package company-c-headers)

;; ;; Configuration for company-c-headers.el
;; ;; The `company-arduino-append-include-dirs' function appends
;; ;; Arduino's include directories to the default directories
;; ;; if `default-directory' is inside `company-arduino-home'. Otherwise
;; ;; just returns the default directories.
;; ;; Please change the default include directories accordingly.
;; ;; macOS: gcc -x c++ -v -E /dev/null
;; ;;  /usr/local/include
;; ;;  /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1
;; ;;  /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/lib/clang/10.0.0/include
;; ;;  /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include
;; ;;  /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.14.sdk/usr/include
;; (defun my-company-c-headers-get-system-path ()
;;   "Return the system include path for the current buffer."
;;   (let ((default '("/usr/include/" "/usr/local/include/")))
;;     (company-arduino-append-include-dirs default t)))
;; (setq company-c-headers-path-system 'my-company-c-headers-get-system-path)

;; (eval-after-load 'company
;;   (add-to-list 'company-backends 'company-c-headers))

;; ;; https://github.com/hotpxl/company-irony-c-headers
;; (use-package company-irony-c-headers)
;; ;; Load with `irony-mode` as a grouped backend
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends '(company-irony-c-headers company-irony)))

;; https://github.com/ZachMassia/platformio-mode
(use-package platformio-mode)

;; https://github.com/Sarcasm/irony-mode
;; Prerequisites
;; irony-server provides the libclang interface to irony-mode. It uses a simple protocol based on S-expression. This server, written in C++ and requires the following packages to be installed on your system:
;; - CMake >= 2.8.3
;; - libclang
(use-package irony)

;; https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc)
(add-hook 'irony-mode-hook #'irony-eldoc)

;; https://github.com/Sarcasm/company-irony
(use-package company-irony)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

;; Enable irony for all c++ files, and platformio-mode only
;; when needed (platformio.ini present in project root).
(add-hook 'c++-mode-hook (lambda ()
                           (irony-mode)
                           (irony-eldoc)
                           (platformio-conditionally-enable)))

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; company-ardunio configuration for irony.el
;; Add arduino's include options to irony-mode's variable.
(add-hook 'irony-mode-hook 'company-arduino-turn-on)

;; Activate irony-mode on arduino-mode
(add-hook 'arduino-mode-hook 'irony-mode)

; Use irony's completion functions.
(add-hook 'irony-mode-hook
          (lambda ()
            (define-key irony-mode-map [remap completion-at-point]
              'irony-completion-at-point-async)

            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)

            (irony-cdb-autosetup-compile-options)))

;; https://github.com/Sarcasm/flycheck-irony
(use-package flycheck-irony)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

(provide 'lang-cpp)

;;; lang-cpp.el ends here
