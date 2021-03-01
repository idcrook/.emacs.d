;;; lang-hardware.el --- Hardware design support, such as verilog

;;; Commentary:

;; SEE ALSO:

;; - [trailbound/yasnippets-systemverilog: SystemVerilog snippets for the yasnippet Emacs package](https://github.com/trailbound/yasnippets-systemverilog)


;;; Code:

;;; https://github.com/veripool/verilog-mode/blob/master/README.adoc#option-2-elpa
;; (use-package verilog-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode)))

(use-package verilog-mode
  :mode
  (("\\.[ds]?vh?\\'" . verilog-mode)))

;; ;; https://github.com/csantosb/vhdl-tools/wiki/Install
;; (use-package vhdl-tools
;;   :init
;;   (autoload 'vhdl-tools-mode "vhdl-tools")
;;   (autoload 'vhdl-tools-vorg-mode "vhdl-tools"))

;; (with-eval-after-load 'vhdl-tools
;;   (require 'vhdl-tools-personal-configuration))


(provide 'lang-hardware)

;;; lang-hardware.el ends here
