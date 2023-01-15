;;; lang-other.el --- Miscellaneous languages

;;; Commentary:

;; SEE ALSO:

;;; Code:


;;; https://github.com/josteink/csharp-mode
;; Emacs 29 includes csharp-mode
(if (version< emacs-version "29.0.50")
    ;;;;; Warning (emacs): csharp-mode is part of Emacs as of Emacs 29 - please delete this package.
    (use-package csharp-mode))


(provide 'lang-other)

;;; lang-other.el ends here
