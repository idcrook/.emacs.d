;;; base-global-keys.el --- Add your keys here
;;
;;; Commentary:

;;; Code:

;(global-set-key (kbd "[SHORTCUT]") '[FUNCTION])

(defun back-to-indentation-or-beginning ()
  "Replace jump-to-beginning with jump-to-indentation."
  (interactive)
 (if (= (point) (progn (back-to-indentation) (point)))
     (beginning-of-line)))

;; https://github.com/vidjuheffex/dotemacs/blob/master/emacs.org#replace-beginning-of-line-with-context-dependent-jump-to-beginning
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

(provide 'base-global-keys)

;;; base-global-keys.el ends here
