;;; base-extensions.el --- Install and configure third-party packages
;;
;;; Commentary:

;;; Code:

(use-package diminish) ; https://github.com/emacsmirror/diminish

;; https://elpa.gnu.org/packages/delight.html
(use-package delight)

;;; http://emacs.stackexchange.com/questions/7432/make-visual-line-mode-more-compatible-with-org-mode
(use-package adaptive-wrap
  :init
  ;; (global-visual-line-mode 1)   ; turns on in all buffers
  (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'text-mode-hook (lambda () (setq line-move-visual nil)))
  (add-hook 'visual-line-mode-hook
	    (lambda ()
	      (adaptive-wrap-prefix-mode +1)
	      (diminish 'visual-line-mode)))
  :config
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (setq-default adaptive-wrap-extra-indent 2))

(use-package ag
  :config
  (add-to-list 'ag-arguments "--hidden" t))

(use-package ansible
  :config
  (setq ansible::vault-password-file (concat user-emacs-directory "vault_pass.txt"))
  (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt))

(use-package ansible-doc)

;; https://github.com/zellio/ansible-vault-mode#automatic-enabling-based-on-file-contents
(defun ansible-vault-mode-maybe ()
  "Turn on `ansible-vault-mode' if the current buffer is an encrypted `ansible-vault' file."
  (when (ansible-vault--is-vault-file)
    (ansible-vault-mode 1)))

(use-package ansible-vault
  :init (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe)
  :config
  (setq ansible-vault-pass-file (expand-file-name "vault_pass.txt" private-dir)))

;; see https://www.slideshare.net/kaz_yos/search-and-replacement-techniques-in-emacs-avy-swiper-multiplecursor-ag-and-wgrep
(use-package avy
  :bind (("C-c SPC" . avy-goto-char)
         (:map isearch-mode-map
	           (("C-'" . avy-isearch)))
         ;; swiper-avy also bound to "C-'"
         )
  :config
  (setq avy-background t
        avy-highlight-first t
        avy-style 'at-full))



;; https://github.com/Malabarba/beacon
(use-package beacon
  :diminish beacon-mode
  :config
  ;; show cursor post-scroll
  (beacon-mode 1)
  (setq beacon-blink-when-point-moves-vertically 1))

;; for bison, yacc, lex grammars, jison mode
(use-package bison-mode)

;; https://github.com/walseb/blimp
(use-package blimp
  :config
  (add-hook 'image-mode-hook 'blimp-mode))

;;; https://github.com/rmuslimov/browse-at-remote/tree/master
;; (browse-at-remote) should open up GitHub, etc. at file in dired, etc
(use-package browse-at-remote
  :bind
  (("C-c b r" . browse-at-remote)))

(use-package browse-url-dwim
  :init
  (setq browse-url-dwim-keystrokes '("C-c b b"))  ;; changed from "C-c b"
  (setq browse-url-dwim-guess-keystrokes '("C-c b g")) ;; changed from "C-c g"
  :config
  (browse-url-dwim-mode 1)
  ;; ;; to turn off confirmations
  ;; (setq browse-url-dwim-always-confirm-extraction nil)
  )

;;; Create and share beautiful images of your source code.
;;; : https://github.com/veelenga/carbon-now-sh.el
(use-package carbon-now-sh)

;;; https://github.com/camdez/checkbox.el
(use-package checkbox
  :bind (("C-c C-c t" . checkbox-toggle)))

(use-package company
  :config
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (add-to-list 'company-backends 'company-ansible)
  (add-to-list 'company-backends 'company-emoji)
  (add-to-list 'company-backends 'company-restclient)
  (add-to-list 'company-backends '(company-shell company-shell-env))
  (add-hook 'after-init-hook 'global-company-mode))

;;; https://github.com/krzysztof-magosa/company-ansible
(use-package company-ansible)

;;; https://github.com/dunn/company-emoji
(use-package company-emoji)

;;; https://github.com/raxod502/prescient.el
(use-package company-prescient
  :requires (prescient)
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1)
  (setq prescient-save-file (expand-file-name "prescient-save.el" temp-dir)))

;;; https://github.com/iquiw/company-restclient
(use-package company-restclient)

;;; https://github.com/Alexander-Miller/company-shell
(use-package company-shell)

;;; https://github.com/sshaw/copy-as-format
(use-package copy-as-format)
(global-set-key (kbd "C-c w s") 'copy-as-format-slack)
(global-set-key (kbd "C-c w g") 'copy-as-format-github)

;; ;; https://github.com/josteink/csharp-mode
;; (use-package csharp-mode)

;; https://github.com/stanaka/dash-at-point
(use-package dash-at-point
  :commands dash-at-point dash-at-point-with-docset
  :bind
  (("C-c d" . dash-at-point)
   ("C-c e" . dash-at-point-with-docset))
  :config
  (add-to-list 'dash-at-point-mode-alist '(web-mode . "html,svg,css,bootstrap,foundation,awesome,javascript,jquery,jqueryui,jquerym,angularjs,backbone,ember,extjs,react,vuejs"))
  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq-local dash-at-point-docset '("Emacs Lisp")))))

;;; https://github.com/rakanalh/emacs-dashboard
(use-package dashboard
  :config
  (setq dashboard-items '((recents       . 5)
                          (projects      . 5)
  ;;                         ;; (bookmarks . 5)
  ;;                         (projects . 5)
  ;;                         ;; (registers . 5)
                          ;; (agenda         . 10)
                            ))
  ;; (add-to-list 'dashboard-items '(agenda) t)

  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)

  (setq dashboard-set-heading-icons t)
  ;; (setq dashboard-set-file-icons t)

  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))


;;  dashboard-project-status is broken with recent dashboard's
;;; https://github.com/functionreturnfunction/dashboard-project-status/issues/2
;; (use-package dashboard
;;   :config
;;   (use-package dashboard-project-status
;;    :config
;;     (add-to-list 'dashboard-item-generators
;;                  `(project-status . ,(dashboard-project-status  (expand-file-name "~/.dotfiles"))))
;;     (add-to-list 'dashboard-items '(project-status) t)
;;     (setq dashboard-items '((project-status . 10)
;;                             (recents        . 5)
;;                             (projects       . 5))))
;;   (dashboard-setup-startup-hook))

;;; https://github.com/Fuco1/dired-hacks
(use-package dired-filter)
;; (use-package dired-rainbow)
;; (use-package dired-subtree)
;; (use-package dired-ranger)
;; (use-package dired-narrow)
;; (use-package dired-list)
(use-package dired-collapse)

;;; https://github.com/Silex/docker.el
(use-package docker)

;;; https://github.com/emacs-pe/docker-tramp.el
(use-package docker-tramp)

;;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode)

;;; built-in
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

;; https://github.com/editorconfig/editorconfig-emacs#readme
(use-package editorconfig
  :diminish editorconfig-mode "EC"
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1)))

;; https://github.com/10sr/editorconfig-custom-majormode-el
(with-eval-after-load 'editorconfig
  (use-package editorconfig-custom-majormode
    :init
    (add-hook 'editorconfig-custom-hooks
              'editorconfig-custom-majormode)))

;; https://github.com/lassik/editorconfig-emacs-domain-specific
(with-eval-after-load 'editorconfig
  (use-package editorconfig-domain-specific
    :init
    (add-hook 'editorconfig-custom-hooks 'editorconfig-domain-specific)))

;; firefox: https://addons.mozilla.org/en-US/firefox/addon/edit-with-emacs1/
;; chrome: https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh
;; https://github.com/stsquad/emacs_chrome
;; for addl ideas see here: https://github.com/stsquad/my-emacs-stuff/blob/master/my-edit-server.el
(use-package edit-server
  :if window-system
  :init
  (when (require 'edit-server nil t)
    ;; do not pop up a new frame
    (setq edit-server-new-frame nil)
    (edit-server-start))
  :config
  (setq edit-server-default-major-mode 'text-mode)
  (setq edit-server-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("stackexchange\\.com" . markdown-mode)
          ("stackoverflow\\.com" . markdown-mode)
          ("reddit\\.com" . markdown-mode))))


(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :init
  (global-flycheck-mode))

;; (use-package flycheck-pos-tip-mode)
;; (with-eval-after-load 'flycheck (flycheck-pos-tip-mode))

;; pip3 install --user yamllint
;; https://github.com/krzysztof-magosa/flycheck-yamllint
(use-package flycheck-yamllint
  :init
  (progn
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-yamllint-setup))))

;;; https://github.com/cadadr/elisp
(use-package forecast
  :config
  (let ((forecast-api-config-file (expand-file-name "forecast-api.config.el" private-dir)))
    (when (file-exists-p forecast-api-config-file)
      (load-file forecast-api-config-file)))
  (let ((forecast-api-key-file (expand-file-name "forecast-api.key.el" private-dir)))
    (when (file-exists-p forecast-api-key-file)
      (load-file forecast-api-key-file)))
  )

;;; https://github.com/defunkt/gist.el
;; broken because of GNU TLS on macOS emacs
;; (use-package gist)

;;; https://github.com/syohex/emacs-git-gutter
(use-package git-gutter
  :diminish git-gutter-mode "gg"
  ;; suggested bindings : https://github.com/syohex/emacs-git-gutter#sample-configuration
  ;; other custom : https://github.com/syohex/emacs-git-gutter/issues/156#issuecomment-394196916
  )

(use-package gitignore-mode)

(use-package graphql-mode)

;; ;; https://www.reddit.com/r/emacs/comments/8vdhb4/tip_how_to_integrate_snippets_with_yasnippets/
;; (use-package hydra
;;   :bind (("C-c y" . hydra-yasnippet/body)))

;; (defhydra hydra-yasnippet (:color blue)
;;   "
;;   ^
;;   ^YASnippet^          ^Do^
;;   ^─────────^──────────^──^────────
;;   _q_ quit             _i_ insert
;;   ^^                   _m_ mode
;;   ^^                   _n_ new
;;   ^^                   ^^
;;   "
;;   ("q" nil)
;;   ("i" yas-insert-snippet)
;;   ("m" yas-minor-mode)
;;   ("n" yas-new-snippet))

;; ________________________________________________________________________
;; Commit to counsel / ivy / swiper as a lifestyle

(use-package ivy
  :demand
  :diminish ivy-mode
  :commands (swiper)
  :bind
  (("C-x s" . swiper)
   ("C-x C-r" . ivy-resume)  ;; find-file-read-only (found in global-map)
   ("C-x B" . ivy-switch-buffer-other-window))
  :config
  (ivy-mode 1)
  (setq
   ivy-use-virtual-buffers t
   ivy-virtual-abbreviate 'fullpath
   ;;ivy-magic-tilde nil ; '~/` instead of just '~'
   ;; hit up arrow when on first line to select contents of prompt line
   ivy-use-selectable-prompt t
   enable-recursive-minibuffers t
   ;; ivy-display-style 'fancy
   ivy-count-format "%d/%d ")
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  ;; from ivy.el
  (ivy-set-actions
   'ivy-switch-buffer
   '(("f"
      ivy--find-file-action
      "find file")
     ("j"
      ivy--switch-buffer-other-window-action
      "other window")
     ("k"
      ivy--kill-buffer-action
      "kill")
     ;; addition: open in another frame
     ("l"
      switch-to-buffer-other-frame
      "other frame")
     ("r"
      ivy--rename-buffer-action
      "rename")))
  )

;;; swiper-isearch
;; (global-set-key (kbd "C-s") 'swiper-isearch)


;;; https://github.com/abo-abo/swiper/wiki/Hiding-dired-buffers
;; hide dired buffers in ivy-switch-buffer
(defun idc/ignore-dired-buffers (str)
  "Return non-nil if STR names a Dired buffer.
This function is intended for use with `ivy-ignore-buffers'."
  (let ((buf (get-buffer str)))
    (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))

(with-eval-after-load 'ivy
  (add-to-list 'ivy-ignore-buffers #'idc/ignore-dired-buffers))

;;; https://github.com/abo-abo/swiper
(use-package ivy-hydra
  :after (ivy hydra))

;;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :requires (counsel)
  :config
  (setq
   ivy-rich-path-style 'abbrev
   ;; whether to parse remote files
   ivy-rich-parse-remote-buffer t      ; default: t
   ivy-rich-parse-remote-file-path t   ; default: nil
   )
  (ivy-rich-mode 1))

(use-package counsel
  :diminish counsel-mode
  :bind
  (("M-x" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x c k" . counsel-yank-pop)         ;; M-y
   ("<f1> f" . counsel-describe-function) ;; C-h f
   ("<f1> v" . counsel-describe-variable) ;; C-h v
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)
   ("<f2> u" . counsel-unicode-char))
  :config
  (counsel-mode))

;;; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :bind
  (("C-x C-a" . counsel-projectile))
  ;; ("C-x c p" . counsel-projectile-ag) ;; default binding is 'C-c p s s'
  :config
  (counsel-projectile-mode +1))



;; https://github.com/nathankot/counsel-dash
;; Browse Dash docsets using Ivy.
;; Requires helm-dash (use-package counsel-dash)


;;;________________________________________________________________________
;; end of counsel / ivy stuff

(use-package jinja2-mode)

;; Emacs 26.1 added this (replaces linum-mode)
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers "%4d \u2502 ")
  ;; ;;(setq display-line-numbers "%4d: ")
  ;;; https://github.com/syohex/emacs-git-gutter/issues/156#issuecomment-395275471
  ;; enable after global-display-line-numbers-mode
  (eval-after-load 'git-gutter
    (global-git-gutter-mode +1)))


;;;________________________________________________________________________
;; Language Server Project (LSP) stuff

;; reference: https://vxlabs.com/2018/11/19/configuring-emacs-lsp-mode-and-microsofts-visual-studio-code-python-language-server/

;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :commands lsp)

;; https://github.com/emacs-lsp/lsp-ui
(use-package lsp-ui
  :commands lsp-ui-mode)
;; (add-hook 'lsp-mode-hook 'lsp-ui-mode)

(use-package company-lsp
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

;; http://immerrr.github.io/lua-mode/
(use-package lua-mode)

(use-package magit
  :config
  (setq magit-completing-read-function 'ivy-completing-read)

  :bind
  ;; Magit
  (("C-x g s" . magit-status)
   ("C-x g x" . magit-checkout)
   ("C-x g c" . magit-commit)
   ("C-x g p" . magit-push)
   ("C-x g u" . magit-pull)
   ("C-x g e" . magit-ediff-resolve)
   ("C-x g r" . magit-rebase-interactive)))

;; based on http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
(defun idc-magit-kill-buffers ()
  "Restore window configuration and kill all (of these) Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

(eval-after-load "magit"
  '(define-key magit-status-mode-map (kbd "Q") #'idc-magit-kill-buffers))

;; https://github.com/alphapapa/magit-todos
;; displays keyword entries from source code comments and Org files in the Magit status buffer.
;;; brew reinstall --with-pcre2 git
;; in magit-status buffer "jT" to jump to TODOs
(use-package magit-todos
  :init
  (magit-todos-mode +1)
  )

;;; https://github.com/jrblevin/markdown-mode
;; C-c C-c p - markdown-preview - open preview in browser
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; for generated html, try to emulate Github README.md previews
  (setq markdown-css-paths '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"))
  (setq markdown-xhtml-body-preamble "<article id=\"markdown-body\" class=\"markdown-body\"><p>")
  (setq markdown-xhtml-body-epilogue "</p></article>")

  ;; https://jblevins.org/log/markdown-imenu
  (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
  (add-hook 'gfm-mode-hook 'imenu-add-menubar-index)
  (setq imenu-auto-rescan t)

  ;;; https://github.com/markedjs/marked
  ;; npm install -g marked  # for marked command
  ;; options emulate Github Flavored Markdown (GFM)
  (setq markdown-command "marked --gfm --breaks --tables")  ; /usr/local/bin/marked
  (setq markdown-gfm-use-electric-backquote nil)
  ;; FIXME: broken under platforms other than macOS
  (setq markdown-open-command "~/bin/macos/marked2"))

;;; https://github.com/ancane/markdown-preview-mode
;; uses markdown-command from markdown-mode
;; dependency:
;; - https://github.com/eschulte/emacs-web-server
;; - https://github.com/ahyatt/emacs-websocket
;; markdown-preview-mode :  start mode and open preview window.
(use-package markdown-preview-mode
  :config
  ;;; Use GFM-like CSS - https://github.com/sindresorhus/github-markdown-css
  (setq markdown-preview-stylesheets (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css"))
  ;; (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")

  ;; (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
  )

;; explicitly rename local repo name for https://github.com/skeeto/emacs-web-server
;; Otherwise aliases to web-server package (emacs-web-server)
(straight-use-package '(simple-httpd :type git :host github :repo "skeeto/emacs-web-server" :local-repo "simple-httpd"))


;;; https://github.com/ardumont/markdown-toc
;; Compute the TOC and insert it at current position: M-x markdown-toc-generate-or-refresh-toc
;; Update the existing TOC: M-x markdown-toc-refresh-toc
(use-package markdown-toc)

;;; https://github.com/nlamirault/emacs-markdownfmt
;; install dependency:
;; go get -u github.com/shurcooL/markdownfmt
(use-package markdownfmt
  :config
  (add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save))

(use-package multiple-cursors
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ;; highlighting symbols only
   ("C-M->" . mc/mark-next-symbol-like-this)
   ("C-M-<" . mc/mark-previous-symbol-like-this)
   ("C-M-*" . mc/mark-all-symbols-like-this)
   ;; highlighting all
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)))

;; https://github.com/domtronn/all-the-icons.el
;; M-x all-the-icons-install-fonts ; puts them in global area
;; neotree uses
(use-package all-the-icons)

;; sidebar and dired in one
(use-package neotree
  :bind
  (("<f8>" . neotree-toggle))
  :config
  ;; needs package all-the-icons
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))

  ;; Disable line-numbers minor mode for neotree
  (add-hook 'neo-after-create-hook
            (lambda (&rest _) (display-line-numbers-mode -1)))

  ;; Every time when the neotree window is opened, let it find current
  ;; file and jump to node.
  (setq neo-smart-open t)

  (setq ;; neo-mode-line-type 'none
        neo-autorefresh nil
        neo-window-width 25
        neo-banner-message nil
        ;; neo-show-hidden-files nil
        ;; neo-keymap-style 'concise
        neo-hidden-regexp-list

        '(;; hidden directories? does not work since does not include '/'
          ;; "^\\..*/$"
          ;; vcs folders
          "^\\.\\(git\\|hg\\|svn\\)$"
          ;; compiled files
          "\\.\\(pyc\\|o\\|elc\\|lock\\|css.map\\)$"
          ;; generated files, caches or local pkgs
          "^\\(node_modules\\|vendor\\|.\\(project\\|cask\\|yardoc\\|sass-cache\\)\\)$"
          ;; org-mode folders
          "^\\.\\(sync\\|export\\|attach\\)$"
          "~$"
          "^#.*#$"))

  ;; track ‘projectile-switch-project’ (C-c p p),
  (setq projectile-switch-project-action 'neotree-projectile-action))

;; NeoTree can be opened (toggled) at projectile project root
(defun idc/neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

;; need another one for python stuff, since this gets re-bound
(global-set-key (kbd "C-c C-p") 'idc/neotree-project-dir)

(defun idc/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun idc/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (idc/neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

;; straight.el relies on internal kludge to build org-mode
(use-package org
  :straight org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :config
  (setq org-directory "~/.org-files"
        org-default-notes-file (concat org-directory "/todo.org"))
  (progn
   (org-babel-do-load-languages
    'org-babel-load-languages
    '((python . t)
      (ruby . t)
      (shell . t)
      (sql . t)
      )))
   ;; (setq org-export-with-sub-superscripts (quote {}))
   ;; (setq (org-src-fontify-natively t)
   )

;; see https://github.com/IvanMalison/org-projectile#use-package
(use-package org-projectile
  :bind (("C-c n p" . org-projectile-project-todo-completing-read)
         ("C-c c" . org-capture))
  :config
  (progn
    (org-projectile-per-project)
    (setq
     org-projectile-per-project-filepath "TODO.org"
     org-agenda-files (append org-agenda-files (org-projectile-todo-files)))))

;;; https://github.com/alf/ob-restclient.el
(use-package ob-restclient)

;;; https://github.com/larstvei/ox-gfm
(use-package ox-gfm)

;;; https://github.com/yjwen/org-reveal
(use-package ox-reveal)

;; https://github.com/abo-abo/org-download
;; Drag and drop images to Emacs org-mode
(use-package org-download
  :init
  (add-hook 'dired-mode-hook 'org-download-enable))

;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook 'org-bullets-mode))

;;; https://www.killring.org/effective-restclient-in-emacs
(use-package outline-magic
  :config
  (add-hook 'outline-minor-mode-hook
	    (lambda ()
	      (require 'outline-magic)
	      (define-key outline-minor-mode-map (kbd "C-<tab>") 'outline-cycle))))

;;; https://github.com/purcell/page-break-lines
(use-package page-break-lines)

;; ________________________________________________________________________
;; projectile-related configs

;; https://github.com/bbatsov/projectile
(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  ;; requires explicit mapping since projectile v1.1
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-known-projects-file
        (expand-file-name "projectile-bookmarks.eld" temp-dir))
  (setq projectile-completion-system 'ivy)
  ;; ignore Archive dirs (similar to .git, .svn, etc.)
  (add-to-list 'projectile-globally-ignored-directories "*Archive")
  (add-to-list 'projectile-globally-ignored-directories "*repos")
  (projectile-mode))

(use-package pcre2el
  :config
  (rxt-global-mode))

;;; https://github.com/openscad/openscad/blob/master/contrib/scad-mode.el
(use-package scad-mode)

(use-package recentf
  :demand
  :init
  (setq recentf-save-file (recentf-expand-file-name "~/.emacs.d/private/cache/recentf"))
  (setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
  :config
  (setq recentf-max-menu-items 22)
  (setq recentf-max-saved-items 500)
  (setq recentf-exclude '("^/var/folders\\.*"
      "COMMIT_EDITMSG\\'"
      ".*-autoloads\\.el\\'"
      "\.emacs\.d\/elpa\/"
      ))
  (recentf-mode 1))

;;; https://github.com/pashky/restclient.el
;; see also: outline-magic
(use-package restclient)

;;; https://github.com/Fuco1/smartparens
;; https://ebzzry.io/en/emacs-pairs/
;; https://github.com/Fuco1/smartparens/wiki
;; (use-package smartparens
;;   :init
;;   (require 'smartparens-config)
;;   :config
;;   (progn (show-smartparens-global-mode t))
;;   (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;;   (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
;;   )


;; (use-package ssh-config-mode)

;; ;; https://github.com/cjohansson/emacs-ssh-deploy
;; (use-package ssh-deploy)

(use-package smooth-scrolling)

;; ;; https://github.com/aaronbieber/sunshine.el
;; ;; requires login for OpenWeatherMap http://api.openweathermap.org/
;; (use-package sunshine
;;   :config
;;   (setq sunshine-location "80538,USA"))

;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :hook (systemd-mode . company-mode))

;;; https://github.com/saf-dmitry/taskpaper-mode
;; taskpaper-mode - Major mode for working with TaskPaper files
(use-package taskpaper-mode
  ;; (add-to-list 'auto-mode-alist '("\\.todo\\'" . taskpaper-mode))
  )

;;; https://github.com/davidshepherd7/terminal-here
(use-package terminal-here
  :config
  (cond
   ((eq system-type 'darwin)
    ;; this works on macOS local for iTerm
    (setq terminal-here-terminal-command (list "open" "-a" "iTerm.app" "."))
    ;; ;; this works on tramp session to a ubuntu remote
    ;; (setq terminal-here-terminal-command (list "/Users/dpc/bin/macos/emacs_term_here_iterm_ssh.py"))
    ))
  :bind
  (("C-<f5>" .  #'terminal-here-launch)
   ("C-<f6>" .  #'terminal-here-project-launch)))

;; https://github.com/koekeishiya/khd/issues/73#issuecomment-298112103
;; <shortcut> : osascript -e 'tell application "iTerm2" to create window with default profile'
;; <shortcut> : osascript -e 'tell application "iTerm2" to create window with default profile command "vim"'

;;; https://github.com/randymorris/tramp-term.el
;; Automatic setup of directory tracking in ssh sessions.
;; FIXME : Doesn't seem to work with my fancy BASH prompt
;; (use-package tramp-term)

;;; https://github.com/snosov1/toc-org
;;  add table of contents to org-mode files (formerly, org-toc)
(use-package toc-org
  :config
  (add-hook 'org-mode-hook 'toc-org-enable))


;; .toml https://github.com/dryman/toml-mode.el
(use-package toml-mode)

;;; Prefer neotree over treemacs, so comment out treemacs
;; https://github.com/Alexander-Miller/treemacs
;; TODO: take another look at treemacs
;; (use-package treemacs
;;   :init
;;   (with-eval-after-load 'winum
;;     (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
;;   :config
;;    (progn
;;     (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
;;           treemacs-file-event-delay           5000
;;           treemacs-follow-after-init          t
;;           treemacs-follow-recenter-distance   0.1
;;           treemacs-goto-tag-strategy          'refetch-index
;;           treemacs-indentation                2
;;           treemacs-indentation-string         " "
;;           treemacs-is-never-other-window      nil
;;           treemacs-no-png-images              nil
;;           treemacs-project-follow-cleanup     nil
;;           treemacs-persist-file               (expand-file-name "treemacs-persist" temp-dir)
;;           treemacs-recenter-after-file-follow nil
;;           treemacs-recenter-after-tag-follow  nil
;;           treemacs-show-hidden-files          t
;;           treemacs-silent-filewatch           nil
;;           treemacs-silent-refresh             nil
;;           treemacs-sorting                    'alphabetic-desc
;;           treemacs-space-between-root-nodes   t
;;           treemacs-tag-follow-cleanup         t
;;           treemacs-tag-follow-delay           1.5
;;           treemacs-width                      35)

;;     (treemacs-follow-mode t)
;;     (treemacs-filewatch-mode t)
;;     (pcase (cons (not (null (executable-find "git")))
;;                  (not (null (executable-find "python3"))))
;;       (`(t . t)
;;        (treemacs-git-mode 'extended))
;;       (`(t . _)
;;        (treemacs-git-mode 'simple))))
;;   :bind
;;   (:map global-map
;;         ("M-0"       . treemacs-select-window)
;;         ("C-x t 1"   . treemacs-delete-other-windows)
;;         ("C-x t t"   . treemacs)
;;         ("C-x t B"   . treemacs-bookmark)
;;         ;;("C-x t C"   . treemacs-show-changelog)
;;         ("C-x t C-t" . treemacs-find-file)
;;         ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-projectile
;;   :after treemacs projectile)

;; ;; https://github.com/lujun9972/verify-url
;; (use-package verify-url)

;; via https://www.veripool.org/projects/verilog-mode/wiki/Installing
;; relies on ~/.emacs.d/user_elisp/verilog-mode.el
;; it appears emacs27 will include verilog-mode
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
(add-to-list 'auto-mode-alist '("\\.[ds]?vh?\\'" . verilog-mode))

;; ;; https://github.com/csantosb/vhdl-tools/wiki/Install
;; (use-package vhdl-tools
;;   :init
;;   (autoload 'vhdl-tools-mode "vhdl-tools")
;;   (autoload 'vhdl-tools-vorg-mode "vhdl-tools"))

;; (with-eval-after-load 'vhdl-tools
;;   (require 'vhdl-tools-personal-configuration))

;;; https://github.com/blak3mill3r/vmd-mode
;;npm install -g vmd
;; ubuntu: sudo apt-get install -y libgconf-2-4
;; ;;; Issue with previewing files edited over TRAMP session
;; ;; https://github.com/blak3mill3r/vmd-mode/issues/16
;; (use-package vmd-mode
;;   :config
;;   (setq vmd-mode--emojis-file  (expand-file-name "github-emojis" temp-dir))
;;   ;;(add-hook 'markdown-mode-hook 'vmd-mode)
;;   ;; run this if needed (vmd-mode--update-emojis-file)
;;   )

;; (eval-after-load "company"
;;   '(defun vmd-company-backend (command &optional arg &rest ignored)
;;   (interactive (list 'interactive))

;;   (cl-case command
;;     (interactive (company-begin-backend 'company-sample-backend))
;;     (prefix (and (eq major-mode 'fundamental-mode)
;;                  (company-grab-symbol)))
;;     (candidates
;;      (cl-remove-if-not
;;       (lambda (c) (string-prefix-p arg c))
;;       vmd-mode-github-emojis-list)))))

;; (add-to-list 'company-backends 'vmd-company-backend)


;; (use-package undo-tree
;;   :config
;;   ;; Remember undo history
;;   (setq
;;    undo-tree-auto-save-history nil
;;    undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
;;   (global-undo-tree-mode 1))

;; https://wakatime.com/emacs
;;     pip3 install wakatime  (macOS with Homebrew / python)
;;     pip3 install --user wakatime
(use-package wakatime-mode
  :config
  ;; (setq wakatime-api-key "...") ;; moved to ~/.wakatime.cfg
  (global-wakatime-mode +1)
  )

(use-package wgrep)

(use-package wgrep-ag)

;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

;;; commenting out as I do not really utilize
;; (use-package windmove
;;   :bind
;;   ("C-x <up>" . windmove-up)
;;   ("C-x <down>" . windmove-down)
;;   ("C-x <left>" . windmove-left)
;;   ("C-x <right>" . windmove-right))

;;; https://github.com/deb0ch/emacs-winum
;; Key binding	Description
;; C-x w <n>	select window <n>, where <n> ranges from 0 to 9. A negative argument deletes the window.
;; C-x w `		select window by number. Number can be given as prefix arg or will be read from minibuffer.
(use-package winum
  :config
  ;; https://github.com/TheBB/spaceline#winum
  (setq winum-auto-setup-mode-line nil)
  (winum-mode))

;;; https://github.com/yoshiki/yaml-mode
(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  )

;;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

;;; https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets)

(provide 'base-extensions)

;;; base-extensions.el ends here
