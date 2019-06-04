;;; base-functions.el --- mostly standalone utility functions

;;; Commentary:

;;; Code:

;; Add your custom functions here

;; (defun something
;;    (do-something))

;;______________________________________________________________________
;;;;  org-babel

;; ;;=
;; ;;; http://orgmode.org/manual/Code-evaluation-security.html

;; (defun my-org-confirm-babel-evaluate (lang body)
;;   (not (string= lang "sql")))  ; don't ask for sql
;; (setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;;______________________________________________________________________
;;;;  markdown support


;; (defun table-gfm-capture (start end)
;;   "convert Markdown table to Emacs table
;; there should be no pipes beginning or ending the line,
;; although this is valid syntax. Loses justification."
;;   (interactive "r")
;;   ;; should prompt user for justification
;;   (table-capture start end "|"
;;          "[\n][:|-]*" 'center))

;; ;; (defun table-gfm-export (start end)
;; ;;   "uses AWK script to convert Emacs table to
;; ;; GFM Markdown table"
;; ;;   (interactive "r")
;; ;;   ;; replace gfm_table_format if necessary
;; ;;   (shell-command-on-region start end "gfm_table_format" t t)
;; ;;   (table-unrecognize))

;; ;;; https://gist.github.com/yryozo/5807243

;; ;; Usage Example:
;; ;;
;; ;; <!-- BEGIN RECEIVE ORGTBL ${1:YOUR_TABLE_NAME} -->
;; ;; <!-- END RECEIVE ORGTBL $1 -->
;; ;;
;; ;; <!--
;; ;; #+ORGTBL: SEND $1 orgtbl-to-gfm
;; ;; | $0 |
;; ;; -->

;; (defun orgtbl-to-gfm (table params)
;;   "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
;;   (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
;;                    org-table-last-alignment ""))
;;      (params2
;;       (list
;;        :splice t
;;        :hline (concat alignment "|")
;;        :lstart "| " :lend " |" :sep " | ")))
;;     (orgtbl-to-generic table (org-combine-plists params2 params))))

;; ;; lazy inserter
;; (defun stag-insert-org-to-md-table (table-name)
;;   (interactive "*sEnter table name: ")
;;   (insert "<!---
;; #+ORGTBL: SEND " table-name " orgtbl-to-gfm

;; -->
;; <!--- BEGIN RECEIVE ORGTBL " table-name " -->
;; <!--- END RECEIVE ORGTBL " table-name " -->")
;;   (previous-line)
;;   (previous-line)
;;   (previous-line))

;;______________________________________________________________________
;;;;  ido recentf support

;; ;;;
;; ;;; http://www.xsteve.at/prg/emacs/power-user-tips.html
;; ;;;
;; ;; Bind M-F6 to a function that uses ido on the recently opened files
;; (defun xsteve-ido-choose-from-recentf ()
;;   "Use ido to select a recently opened file from the `recentf-list'"
;;   (interactive)
;;   (let ((home (expand-file-name (getenv "HOME"))))
;;     (find-file
;;      (ido-completing-read "Recentf open: "
;;                           (mapcar (lambda (path)
;;                                     (replace-regexp-in-string home "~" path))
;;                                   recentf-list)
;;                           nil t))))

;; ;;
;; (global-set-key (kbd "M-<f6>") 'xsteve-ido-choose-from-recentf)


;;______________________________________________________________________
;;;;  miscellaneous

;; ;; http://emacs.stackexchange.com/questions/6051/fixing-forced-indentation-
;; (defun now ()
;;   "Insert string for the current time formatted like '2:34 PM'."
;;   (interactive)                 ; permit invocation in minibuffer
;;   (insert (format-time-string "%D %-I:%M %p - ")))

;; (defun today ()
;;   "Insert string for today's date nicely formatted in American style,
;; e.g. Sunday, September 17, 2000."
;;   (interactive)                 ; permit invocation in minibuffer
;;   (insert (format-time-string "%A, %B %e, %Y")))


;; ;; show ascii table
;; ;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
;; (defun ascii-table ()
;;   "Print the ascii table. Based on a defun by Alex Schroeder <asc@bsiag.com>"
;;   (interactive)
;;   (switch-to-buffer "*ASCII*")
;;   (erase-buffer)
;;   (insert (format "ASCII characters up to number %d.\n" 254))
;;   (let ((i 0))
;;     (while (< i 254)
;;       (setq i (+ i 1))
;;       (insert (format "%4d %c\n" i i))))
;;   (beginning-of-buffer))

;; ;;http://db.glug-bom.org/Documentation/a-dot-emacs-file.txt
;; ; From Philip Lijnzaad <lijnzaad@ebi.ac.uk> in gnu.emacs.help
;; ; Functions to switch dos/mac/unix modes
;; (defun dos-line-endings ()
;;   "sets the buffer-file-coding-system to undecided-dos; changes the buffer
;;     by invisibly adding carriage returns"
;;   (interactive)
;;   (set-buffer-file-coding-system 'undecided-dos nil))

;; (defun unix-line-endings ()
;;   "sets the buffer-file-coding-system to undecided-unix; changes the buffer
;;     by invisibly removing carriage returns"
;;   (interactive)
;;   (set-buffer-file-coding-system 'undecided-unix nil))

;; (defun mac-line-endings ()
;;   "sets the buffer-file-coding-system to undecided-mac; may change the buffer
;;     by invisibly removing carriage returns"
;;   (interactive)
;;   (set-buffer-file-coding-system 'undecided-mac nil))

;;; https://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Take multi-line paragraph(s) (or REGION) and turn into a single lines of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))


(provide 'base-functions)

;;; base-functions.el ends here
