;;; lang-cpp.el --- C++ and C language dialects integration

;;; Commentary:

;; SEE ALSO: lang-apple.el

;;; Code:


(require 'cc-vars)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "linux")))  ;; default value of "gnu"

(setq-default c-basic-offset                     4)
(setq         c-basic-offset                     4)

;;; https://github.com/Lindydancer/cmake-font-lock
(use-package cmake-font-lock)

;;; https://github.com/chachi/cuda-mode
(use-package cuda-mode)
;; (push 'cuda-mode irony-supported-major-modes)

(setq cuda-mode-hook nil)

;; add path manually; FIXME: alternatively obtain from CMake database .json
(add-hook 'cuda-mode-hook
          (lambda ()
            ( setq c-basic-offset              4
                   flycheck-cuda-include-path (list
                                               "/usr/local/nvidia/NVIDIA-OptiX-SDK-6.5.0-linux64/include"
                                               ;; (expand-file-name "~/projects/learning/rt/rt_optix/src/OptiX/RestOfLife")
                                               ".")
                   flycheck-cuda-explicitly-specify-cuda-language t)))

;;; https://github.com/Sarcasm/irony-mode
;;
;; Prerequisites:
;;
;; irony-server provides the libclang interface to irony-mode. It uses a simple
;; protocol based on S-expression. This server, written in C++ and requires the
;; following packages to be installed on your system:
;;
;; - CMake >= 2.8.3
;; - libclang # provided by llvm via Xcode on macOS, need to install libclang
;;              headers (see below)

(use-package irony)

;; ubuntu: sudo apt install cmake libclang1 libclang-dev
;; macos: brew install cmake
;;
;; Note: since relies on Homebrew, assumes macOS user already owns /usr/local
;;
;;     cp -p "`xcode-select --print-path`"/Toolchains/XcodeDefault.xctoolchain/usr/lib/libclang.dylib /usr/local/lib
;;     cd /tmp
;;     svn export http://llvm.org/svn/llvm-project/cfe/trunk/include/clang-c/
;;     cp -RP clang-c /usr/local/include
;;
;; M-x irony-install-server

;; Enable irony for all c++ files
(add-hook 'c++-mode-hook (lambda ()
                           (irony-mode)
                           (irony-eldoc)
                           (setq flycheck-gcc-language-standard "c++11")))

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun dpc-irony-mode-hook ()
  "Change the \\<C-M-i> binding to counsel-irony."
  (define-key irony-mode-map
    [remap completion-at-point] 'counsel-irony)
  (define-key irony-mode-map
    [remap complete-symbol] 'counsel-irony))

; Use irony's completion functions.
(add-hook 'irony-mode-hook 'dpc-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; (add-hook 'irony-mode-hook
;;           (lambda ()
;;             (define-key irony-mode-map [remap completion-at-point]
;;               'irony-completion-at-point-async)
;;             (define-key irony-mode-map [remap complete-symbol]
;;               'irony-completion-at-point-async)
;;             (irony-cdb-autosetup-compile-options)
;;             ))

;; https://github.com/Sarcasm/flycheck-irony
(use-package flycheck-irony)

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; https://github.com/ikirill/irony-eldoc
(use-package irony-eldoc)
(add-hook 'irony-mode-hook #'irony-eldoc)

;; ubuntu: sudo apt install clang
;; https://github.com/Sarcasm/company-irony
(use-package company-irony)
;;; load grouped with company-irony-c-headers below
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-irony))

;; https://github.com/hotpxl/company-irony-c-headers
(use-package company-irony-c-headers)
;; Load with `irony-mode` as a grouped backend
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; Sometimes when the compiler options change, you need to manually reload
;; header completion cache by invoking
;; company-irony-c-headers-reload-compiler-output.


;; https://github.com/stardiviner/arduino-mode
;; (use-package arduino-mode)

;; company-ardunio configuration for irony.el
;; Add arduino's include options to irony-mode's variable.
;; (add-hook 'irony-mode-hook 'company-arduino-turn-on)

;; Activate irony-mode on arduino-mode
;; (add-hook 'arduino-mode-hook 'irony-mode)

;; https://github.com/yuutayamada/company-arduino
;; (use-package company-arduino)  ;; requires:  irony-mode, company-irony and company-c-headers
;; Note
;; This package's default configuration is set for Linux environment, which I'm currently using, so if you are using different environment (Mac or Windows), please change the Arduino's directory paths accordingly. Related variables: company-arduino-sketch-directory-regex, company-arduino-home, company-arduino-header, company-arduino-includes-dirs and irony-arduino-includes-options.

;; https://github.com/randomphrase/company-c-headers
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

;; https://github.com/ZachMassia/platformio-mode
;; (use-package platformio-mode)



(provide 'lang-cpp)

;;; lang-cpp.el ends here
