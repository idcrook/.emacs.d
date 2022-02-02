;;; lang-lisp.el --- Common Lisp

;;; Commentary:

;; SEE ALSO:

;;; Code:

;;; Implementations
;; Steel Bank Common Lisp (SBCL) - high-performance - `brew install sbcl`
;; Clozure Common Lisp (CCL) - fast compile - `brew install clozure-cl`
;; Embedded Common Lisp (ECL) - embeddable `brew install ecl`
;; CNU CLISP - `brew install clisp`
;; clasp - LLVM/C++ https://github.com/clasp-developers/clasp
;;  - Currently there are no binary releases available

;;; Libraries
;; quicklisp - https://www.quicklisp.org/beta/
;; - postmodern - postgres
;; - etc.

;;; SLIME
(use-package slime
  :init
  (setq slime-contribs '(slime-fancy slime-quicklisp slime-asdf))
  :config
  (setq inferior-lisp-program "sbcl")
)

(provide 'lang-lisp)

;;; lang-lisp.el ends here
