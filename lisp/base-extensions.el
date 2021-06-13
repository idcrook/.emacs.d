;;; base-extensions.el --- Install and configure third-party packages
;;
;;; Commentary:

;;; Code:

;;; https://github.com/emacsmirror/diminish
(use-package diminish)

;;; https://elpa.gnu.org/packages/delight.html
(use-package delight)

;;; [jschaf/esup: ESUP - Emacs Start Up Profiler](https://github.com/jschaf/esup)
(use-package esup)

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

;;; https://github.com/Wilfred/ag.el#readme
(when (executable-find "ag")
  (use-package ag
    :config
    (setq-default ag-highlight-search t)
    (add-to-list 'ag-arguments "--hidden" t))
  (use-package wgrep
    :init
    (setq wgrep-enable-key "p"))
  (use-package wgrep-ag))

;; (use-package ansible
;;   :config
;;   (setq ansible::vault-password-file (concat user-emacs-directory "vault_pass.txt"))
;;   (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt))
;; (use-package ansible-doc)
;; ;;; https://github.com/zellio/ansible-vault-mode#automatic-enabling-based-on-file-contents
;; (defun ansible-vault-mode-maybe ()
;;   "Turn on `ansible-vault-mode' if the current buffer is an encrypted `ansible-vault' file."
;;   (when (ansible-vault--is-vault-file)
;;     (ansible-vault-mode 1)))
;; (use-package ansible-vault
;;   :init (add-hook 'yaml-mode-hook 'ansible-vault-mode-maybe)
;;   :config
;;   (setq ansible-vault-pass-file (expand-file-name "vault_pass.txt" private-dir)))

;;; https://github.com/sensorflo/adoc-mode
(use-package adoc-mode
  :mode (("\\.adoc$" . adoc-mode)))

(use-package avy
  :bind (("C-c SPC" . avy-goto-char)
         :map isearch-mode-map
	           ("C-'" . avy-isearch))
         ;; swiper-avy also bound to "C-'"
  :config
  (setq avy-background t
        avy-highlight-first t
        avy-style 'at-full))

;;; https://github.com/Malabarba/beacon
(use-package beacon
  :diminish beacon-mode
  :config
  ;; show cursor post-scroll
  (beacon-mode 1)
  (setq beacon-blink-when-point-moves-vertically 1))

;; ;; for bison, yacc, lex grammars, jison mode
;; (use-package bison-mode)

;; ;;; https://github.com/walseb/blimp
;; ;; a complete wrapper around all imagemagick commands (requires it)
;; (use-package blimp
;;   :config
;;   (add-hook 'image-mode-hook 'blimp-mode))

;;; https://github.com/rmuslimov/browse-at-remote
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

;; ;; Create and share beautiful images of your source code.
;; ;;; https://github.com/veelenga/carbon-now-sh.el
;; (use-package carbon-now-sh)

;;; https://github.com/camdez/checkbox.el
(use-package checkbox
  :bind (("C-c C-c t" . checkbox-toggle)))

(use-package company
  :config
  (setq company-idle-delay 0.2)
  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)
  (add-hook 'after-init-hook 'global-company-mode))

;; ;;; https://github.com/krzysztof-magosa/company-ansible
;; (use-package company-ansible
;;   :init
;;   (add-to-list 'company-backends 'company-ansible))

;; ;;; https://github.com/dunn/company-emoji
;; (use-package company-emoji
;;   :init
;;   (add-to-list 'company-backends 'company-emoji))

;;; https://github.com/raxod502/prescient.el
(use-package company-prescient
  :requires (prescient)
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1)
  (setq prescient-save-file (expand-file-name "prescient-save.el" temp-dir)))

;;; https://github.com/iquiw/company-restclient
(use-package company-restclient
;;  :defer 2
  :init
  (add-to-list 'company-backends 'company-restclient))

;;; https://github.com/sshaw/copy-as-format
(use-package copy-as-format)
;; (global-set-key (kbd "C-c w s") 'copy-as-format-slack)
(global-set-key (kbd "C-c w o") 'copy-as-format-org-mode)
(global-set-key (kbd "C-c w g") 'copy-as-format-github)

;;; https://elpa.gnu.org/packages/csv-mode.html
(use-package csv-mode)

;;; https://github.com/rakanalh/emacs-dashboard
;; - needs package all-the-icons (and icons installed)
(use-package dashboard
  :after (all-the-icons)
  :config
  (setq dashboard-items '((recents       . 5)
  ;;                        (bookmarks . 5)
                          ;; (projects . 5)
  ;;                         (registers . 5)
  ;;                        (agenda         . 10)
                            ))
  ;; (add-to-list 'dashboard-items '(agenda) t)
  ;; ;; To show info about the packages loaded and the init time:
  ;; (setq dashboard-set-init-info t)
  ;; To show navigator below the banner:
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
      `(;; line1
        ((,(all-the-icons-octicon "mark-github" :height 1.1 :v-adjust 0.0)
         "Homepage"
         "Browse homepage"
         (lambda (&rest _) (browse-url "https://github.com/idcrook")))
        ;; ("★" "Star" "Show stars" (lambda (&rest _) (show-stars)) warning)
        ;; ("?" "" "?/h" #'show-help nil "<" ">")
        )
         ;; line 2
        ;; ((,(all-the-icons-faicon "linkedin" :height 1.1 :v-adjust 0.0)
        ;;   "Linkedin"
        ;;   ""
        ;;   (lambda (&rest _) (browse-url "homepage")))
        ;;  ("⚑" nil "Show flags" (lambda (&rest _) (message "flag")) error))
        ))
  ;; Content is not centered by default. To center, set
  (setq dashboard-center-content t)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  ;; Set the title
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
  ;; Set the banner
  (setq dashboard-startup-banner 'logo)
  ;; A randomly selected footnote will be displayed. To disable it:
  (setq dashboard-set-footer nil)
  ;; (setq dashboard-footer-messages '("Dashboard is pretty cool!"))
  ;; (setq dashboard-footer-icon (all-the-icons-octicon "dashboard"
  ;;                                                    :height 1.1
  ;;                                                    :v-adjust -0.05
  ;;                                                    :face 'font-lock-keyword-face))
  (dashboard-setup-startup-hook))

;;; https://github.com/Fuco1/dired-hacks
(use-package dired-filter)
;; (use-package dired-rainbow)
;; (use-package dired-subtree)
;; (use-package dired-ranger)
;; (use-package dired-narrow)
;; (use-package dired-list)
(use-package dired-collapse)

;; ;;; https://github.com/Silex/docker.el
;; (use-package docker)

;; ;;; https://github.com/emacs-pe/docker-tramp.el
;; (use-package docker-tramp)

;;; https://github.com/spotify/dockerfile-mode
(use-package dockerfile-mode)

;;; built-in
(use-package ediff
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq-default ediff-highlight-all-diffs 'nil)
  (setq ediff-diff-options "-w"))

;;; https://github.com/editorconfig/editorconfig-emacs#readme
(use-package editorconfig
  :diminish editorconfig-mode "EC"
  :init
  (add-hook 'prog-mode-hook (editorconfig-mode 1))
  (add-hook 'text-mode-hook (editorconfig-mode 1))
  (add-hook 'editorconfig-hack-properties-functions
            '(lambda (props)
               (when (derived-mode-p 'makefile-mode)
                 (puthash 'indent_style "tab" props))))
  ;;; https://github.com/10sr/editorconfig-custom-majormode-el
  (with-eval-after-load 'editorconfig
    (use-package editorconfig-custom-majormode
      :init
      (add-hook 'editorconfig-custom-hooks
                'editorconfig-custom-majormode)))
  ;;; https://github.com/lassik/editorconfig-emacs-domain-specific
  (with-eval-after-load 'editorconfig
    (use-package editorconfig-domain-specific
;;      :defer 2
      :init
      (add-hook 'editorconfig-custom-hooks 'editorconfig-domain-specific))))

;; ;;; firefox: https://addons.mozilla.org/en-US/firefox/addon/edit-with-emacs1/
;; ;;; chrome: https://chrome.google.com/webstore/detail/edit-with-emacs/ljobjlafonikaiipfkggjbhkghgicgoh
;; ;;; https://github.com/stsquad/emacs_chrome
;; ;;; for addl ideas see here: https://github.com/stsquad/my-emacs-stuff/blob/master/my-edit-server.el
;; (use-package edit-server
;;   :if window-system
;;   :init
;;   (when (require 'edit-server nil t)
;;     ;; do not pop up a new frame
;;     (setq edit-server-new-frame nil)
;;     (edit-server-start))
;;   :config
;;   (setq edit-server-default-major-mode 'text-mode)
;;   (setq edit-server-url-major-mode-alist
;;         '(("github\\.com" . gfm-mode)
;;           ("stackexchange\\.com" . markdown-mode)
;;           ("stackoverflow\\.com" . markdown-mode)
;;           ("reddit\\.com" . markdown-mode))))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

;;; https://www.flycheck.org/en/latest/
(use-package flycheck
  :init
  (global-flycheck-mode))

;;; https://github.com/flycheck/flycheck-pos-tip#readme
(use-package flycheck-pos-tip
  :init
  (with-eval-after-load 'flycheck (flycheck-pos-tip-mode)))

;; ;;; https://github.com/cadadr/elisp
;; (use-package forecast
;;   :config
;;   (let ((forecast-api-config-file (expand-file-name "forecast-api.config.el" private-dir)))
;;     (when (file-exists-p forecast-api-config-file)
;;       (load-file forecast-api-config-file)))
;;   (let ((forecast-api-key-file (expand-file-name "forecast-api.key.el" private-dir)))
;;     (when (file-exists-p forecast-api-key-file)
;;       (load-file forecast-api-key-file)))
;;   )

;;; https://github.com/defunkt/gist.el
(use-package gist)
;;  :defer 2)

;;; https://github.com/syohex/emacs-git-gutter
(use-package git-gutter
  ;; :diminish git-gutter-mode "gg"
  :diminish git-gutter-mode
  ;; suggested bindings : https://github.com/syohex/emacs-git-gutter#sample-configuration
  ;; other custom : https://github.com/syohex/emacs-git-gutter/issues/156#issuecomment-394196916
  :bind
  (("C-x C-g" . git-gutter)))

;; Emacs 26.1 added this (replaces linum-mode)
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode 1)
  (setq display-line-numbers "%4d \u2502 ")
  ;; ;;(setq display-line-numbers "%4d: ")
  ;;; https://github.com/syohex/emacs-git-gutter/issues/156#issuecomment-395275471
  ;; enable after global-display-line-numbers-mode
  (eval-after-load 'git-gutter
    (global-git-gutter-mode +1)))

;;; https://github.com/magit/git-modes#gitconfig-mode
(use-package gitconfig-mode)

;;; https://github.com/magit/git-modes#gitignore-mode
(use-package gitignore-mode)

;;----------------------------------------------------------------------------
;; counsel / ivy / swiper is a lifestyle
;;----------------------------------------------------------------------------
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

;; ;;; https://github.com/abo-abo/swiper
;; (use-package ivy-hydra
;;   :after (ivy hydra))

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
   ("<f1> o" . counsel-describe-symbol)   ;; C-h o
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char)
   ;; ("C-c g" . counsel-git) ;; use for magit-file-dispatch instead
   ("C-c C-f" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag))
  :config
  (counsel-mode))

;;; [Use ivy interface to access your firefox bookmarks and history in Emacs](https://github.com/cireu/counsel-ffdata)
(use-package counsel-ffdata
  :bind
  (("C-c F h" . counsel-ffdata-firefox-history)
   ("C-c F b" . counsel-ffdata-firefox-bookmarks))
  :config
  (if (eq system-type 'darwin)
      ;; TODO: dynamically find the places.sqlite?
      ;; https://github.com/cireu/counsel-ffdata/issues/3
      (setq counsel-ffdata-database-path "/Users/dpc/Library/Application Support/Firefox/Profiles/ke986qv7.default-release/places.sqlite"))
  )

;;; https://github.com/200ok-ch/counsel-jq
(use-package counsel-jq)
;; FIXME: add binding for counsel-jq in json buffers

;;; https://github.com/masasam/emacs-counsel-tramp
(use-package counsel-tramp
  :bind
  (("C-c s" . counsel-tramp))
  :init
  ;; to speed up tramp
  (add-hook 'counsel-tramp-pre-command-hook '(lambda ()
                                               ;;				     (projectile-mode 0)
				                               (editorconfig-mode 0)))
  (add-hook 'counsel-tramp-quit-hook '(lambda ()
                                        ;;			      (projectile-mode 1)
			                            (editorconfig-mode 1)))
  ;; ;; If the shell of the server is zsh it is recommended to connect with bash.
  ;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
  :config
  (setq counsel-tramp-custom-connections
        '(;; /ssh:rpih1|sudo:root@rpih1:/etc/shairport-sync.conf
          ;; /ssh:rpih1|sudo:root@rpih1:/etc/mosquitto/credentials/aclfile
          /ssh:rpihp2:projects/kubernetes-homespun/RUN.md
          )))

;;; https://github.com/mnewt/counsel-web
(use-package counsel-web
;;  :defer 2
  ;; :bind
  ;; (("C-c w" . counsel-web-suggest)
  ;;  ("C-c W" . counsel-web-search)
  ;;  ("C-c C-w" . counsel-web-thing-at-point))
  :config
  ;; Define "C-c w" as a prefix key.
  (defvar counsel-web-map
    (let ((map (make-sparse-keymap "counsel-web")))
      (define-key map (kbd "w") #'counsel-web-suggest)
      (define-key map (kbd "s") #'counsel-web-search)
      (define-key map (kbd ".") #'counsel-web-thing-at-point)
      map))
  (global-set-key (kbd "C-c w") counsel-web-map)
  ;; use google instead of the default DuckDuckGo
  (setq counsel-web-engine 'google)
  (setq ;; update with each key press
        counsel-web-search-dynamic-update  t
        ;; use system browser
        counsel-web-search-action          #'browse-url))

;;----------------------------------------------------------------------------
;; end of counsel / ivy stuff
;;----------------------------------------------------------------------------

;;; https://github.com/redguardtoo/find-file-in-project
;; in lieu of projectile, for example: #'ffip-get-project-root-directory
(use-package find-file-in-project)

;; ;;; https://github.com/paradoxxxzero/jinja2-mode/
;; ;; FIXED: bug with severe effects ;; https://github.com/paradoxxxzero/jinja2-mode/issues/18
;; (use-package jinja2-mode)

;; ;;; http://immerrr.github.io/lua-mode/
;; (use-package lua-mode
;;   :init
;;   (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
;;   (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;----------------------------------------------------------------------------
;; magit
;;----------------------------------------------------------------------------
(use-package magit
  :init
  (eval-after-load "magit"
    '(define-key magit-status-mode-map (kbd "Q") #'idc-magit-kill-buffers))
  (setq magit-define-global-key-bindings nil)
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; ;; When commiting enable verbose mode by default.
  ;; (setq magit-commit-arguments (quote ("--verbose")))
  ;; (global-set-key (kbd "C-c g") 'magit-file-dispatch)
  :bind
  ;; Magit
  (("C-x g s" . magit-status)         ;; global default is "C-x g"
   ("C-x M-g" . magit-dispatch)       ;; same as global default
   ("C-c g"   . magit-file-dispatch)  ;; global default is "C-c M-g"
   ("C-x g x" . magit-checkout)
   ("C-x g c" . magit-commit)
   ("C-x g p" . magit-push)
   ("C-x g u" . magit-pull)
   ("C-x g e" . magit-ediff-resolve)
   ("C-x g r" . magit-rebase-interactive)))

;;; based on http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
(defun idc-magit-kill-buffers ()
  "Restore window configuration and kill all (of these) Magit buffers."
  (interactive)
  (let ((buffers (magit-mode-get-buffers)))
    (magit-restore-window-configuration)
    (mapc #'kill-buffer buffers)))

;; ;;; https://github.com/ragone/magit-diff-flycheck
;; ;; Run M-x magit-diff-flycheck in a magit-diff buffer to display a filtered
;; ;; list of Flycheck errors for the added/modified lines only.
;; (use-package magit-diff-flycheck)

;;; https://github.com/alphapapa/magit-todos
;; displays keyword entries from source code comments and Org files in the Magit status buffer.
;; can use: M-x magit-todos-list  or  M-x ivy-magic-todos
(use-package magit-todos
;;  :defer 2
  :after (magit)
  :config
  ;; ;; magit-todos: Not overriding bind of "jT" in ‘magit-status-mode-map’.
  ;; (magit-todos-mode +1)
  (let ((inhibit-message t))
    (magit-todos-mode +1))
  ;; BROKEN - in magit-status buffer "j T" to jump to TODOs
  ;;; workaround: https://github.com/alphapapa/magit-todos/issues/95#issuecomment-654006672
  ;;; patch: https://github.com/wyuenho/magit-todos/commit/ed4f5c49d726919529ef621dde56330dea3653a6
  (transient-append-suffix
    'magit-status-jump
    '(0 -1) ;; places in a new third column
    '[("T " "Todos" magit-todos-jump-to-todos)
      ("l " "List todos" ivy-magit-todos)])
  )
;;    '(0 0 -1)
;;    '("T " "Todos" magit-todos-jump-to-todos)))

;;; https://github.com/jrblevin/markdown-mode
;; C-c C-c p - markdown-preview - open preview in browser
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . gfm-mode)
         ("\\.md.html\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  ;; for generated html, try to emulate Github README.md previews
  (setq markdown-css-paths '("https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/4.0.0/github-markdown.min.css"))
  (setq markdown-xhtml-body-preamble "<article id=\"markdown-body\" class=\"markdown-body\"><p>")
  (setq markdown-xhtml-body-epilogue "</p></article>")

  ;;; https://jblevins.org/log/markdown-imenu
  (add-hook 'markdown-mode-hook 'imenu-add-menubar-index)
  (add-hook 'gfm-mode-hook 'imenu-add-menubar-index)
  (setq imenu-auto-rescan t)

  ;;; https://github.com/markedjs/marked
  ;; npm install -g marked  # for marked command
  ;; /usr/local/bin/marked or nvm path
  ;; options emulate Github Flavored Markdown (GFM)
  (setq markdown-command "marked --gfm --breaks --tables")
  (setq markdown-gfm-use-electric-backquote nil)
  ;; FIXME: broken under platforms other than macOS
  (setq markdown-open-command "~/bin/macos/marked2"))

;;; https://github.com/ancane/markdown-preview-mode
  ;; uses markdown-command from markdown-mode
;; dependencies:
;; - markdown-mode
;; - https://github.com/eschulte/emacs-web-server
;; - https://github.com/ahyatt/emacs-websocket
;; markdown-preview-mode :  start mode and open preview window.
(use-package markdown-preview-mode
  ;; Make a keybinding: `C-c C-c l'
  ;; - overrides markdown-live-preview-mode default binding
  :bind (:map markdown-mode-command-map
              ("l" . markdown-preview-mode))
  :config
  ;;; Use GFM-like CSS - https://github.com/sindresorhus/github-markdown-css
  (setq markdown-preview-stylesheets (list "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/4.0.0/github-markdown.min.css"))
  ;; Add extra css to default solarized dark theme
  (add-to-list 'markdown-preview-stylesheets "https://raw.githubusercontent.com/richleland/pygments-css/master/emacs.css")
  ;; Add MathJax
  (add-to-list 'markdown-preview-javascript "http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML")
  )

;; explicitly rename local repo name for https://github.com/skeeto/emacs-web-server
;; Otherwise aliases to web-server package (emacs-web-server)
(straight-use-package '(simple-httpd :type git :host github :repo "skeeto/emacs-web-server" :local-repo "simple-httpd"))

;; ;;; https://github.com/seagle0128/grip-mode
;; ;; GFM/Org preview using Grip (GitHub Readme Instant Preview).
;; ;; macOS/ubuntu:      pip3 install --user grip
;; ;;     raspbian: sudo pip3 install --upgrade grip
;; ;;
;; ;; :hook ((markdown-mode org-mode) . grip-mode)
;; ;;
;; ;; Filed issue to see if grip-mode can work for Tramp files
;; ;;     https://github.com/seagle0128/grip-mode/issues/5 <- UPDATED!
;; (use-package grip-mode
;;   :init
;;   ;; user name and password from ~/.authinfo ;; FORMAT:
;;   ;; machine api.github.com login YOU password YOURPASSWORD
;;   (let ((credential (auth-source-user-and-password "api.github.com")))
;;     (setq grip-github-user (car credential)
;;           grip-github-password (cadr credential)))
;;   (setq grip-update-after-change nil)
;;   (setq grip-preview-use-webkit nil)
;;   ;; Make a keybinding: `C-c C-c g'
;;   :bind (:map markdown-mode-command-map
;;               ("g" . grip-mode)))

;;; https://github.com/ardumont/markdown-toc
;; Compute the TOC and insert it at current position: M-x markdown-toc-generate-or-refresh-toc
;; Update the existing TOC: M-x markdown-toc-refresh-toc
(use-package markdown-toc
  :config
  ;; https://github.com/ardumont/markdown-toc#user-toc-manipulation
  ;; drop the first element (since this is usually the title):
  (custom-set-variables '(markdown-toc-user-toc-structure-manipulation-fn 'cdr)))

;;; https://github.com/nlamirault/emacs-markdownfmt
;; install dependency:
;; # does not support front matter # go get -u github.com/shurcooL/markdownfmt
;; # supports frontmatter #  GO111MODULE=on  go get -v github.com/moorereason/mdfmt
(use-package markdownfmt
  :init
  (setq markdownfmt-bin "mdfmt")  ;; default: "markdownfmt"
  ;; make a binding "C-c C-c f"
  :bind (:map markdown-mode-command-map
              ("f" . markdownfmt-format-buffer))
  ;; (define-key markdown-mode-map (kbd "C-c C-c f") #'markdownfmt-format-buffer))
  ;; :config
  ;; ;; uncomment to autoformat ;; (add-hook 'markdown-mode-hook #'markdownfmt-enable-on-save)
  )

;; (use-package multiple-cursors
;;   :bind
;;   (("C-S-c C-S-c" . mc/edit-lines)
;;    ;; highlighting symbols only
;;    ("C-M->" . mc/mark-next-symbol-like-this)
;;    ("C-M-<" . mc/mark-previous-symbol-like-this)
;;    ("C-M-*" . mc/mark-all-symbols-like-this)
;;    ;; highlighting all
;;    ("C->" . mc/mark-next-like-this)
;;    ("C-<" . mc/mark-previous-like-this)
;;    ("C-c C->" . mc/mark-all-like-this)))

;; https://github.com/domtronn/all-the-icons.el
;; M-x all-the-icons-install-fonts ; puts them in global area
;; dashboard uses
(use-package all-the-icons)

;;; https://orgmode.org/worg/org-contrib/
(use-package org-contrib)
;; (straight-use-package '(org :type built-in))
;; (straight-use-package '(org-contrib :includes ol-vm))

;; TODO investigate
;; - ivy-todo
;; - magit-org-todos
;; - ob-rust, ob-uart
;; - org-capture-pop-frame
;; - org-kanban, org-mobile-sync , org-trello
;; - org-protocol-jekyll, org2jekyll, org2issue
;; - org-shoplist, org-rtm
;; - org-re-reveal, ox-reveal, ox-hugo, ox-epub, ox-jekyll-md, ox-clip

;; ;;; [org-opml/org-opml: Edit OPML files using Org mode in Emacs](https://github.com/org-opml/org-opml)
;; NOTE: Was able to get loaded after updating :files directive; OPML parsing
;;     is not robust to different generators, and was breaking "round-trip"
;; (use-package ox-opml
;;   :straight (ox-opml :type git :host github
;;                      :repo "org-opml/org-opml"
;;                      ))

;; (use-package org-opml
;;   :straight (org-opml :type git :host github
;;                       :repo "org-opml/org-opml"
;; ;; opml-decode: Could not locate opml2org.py. Make sure it’s in ‘load-path’.
;;                       :files (:defaults "opml2org.py")
;;                       ))


;;; https://github.com/yashi/org-asciidoc
(use-package ox-asciidoc
  :after (org))

;; ;;; https://github.com/larstvei/ox-gfm
;; (use-package ox-gfm
;;   :after (org))
;; (eval-after-load "org"
;;   '(require 'ox-gfm nil t))

;; ;;; https://github.com/yjwen/org-reveal
;; (use-package ox-reveal)

;; ;;; https://github.com/abo-abo/org-download
;; ;; Drag and drop images to Emacs org-mode
;; (use-package org-download
;;   :init
;;   (add-hook 'dired-mode-hook 'org-download-enable))

;; https://github.com/emacsorphanage/org-bullets
(use-package org-bullets
;;  :defer 2
  :config
  (setq org-hide-leading-stars t)
  (add-hook 'org-mode-hook 'org-bullets-mode))

;; ;;; https://github.com/snosov1/toc-org
;; ;; add table of contents to org-mode files (formerly, org-toc)
;; (use-package toc-org
;;   :config
;;   (add-hook 'org-mode-hook 'toc-org-enable))

;; ;;; https://github.com/alf/ob-restclient.el
;; (use-package ob-restclient)

;; ;;; https://www.killring.org/effective-restclient-in-emacs
;; (use-package outline-magic
;;   :config
;;   (add-hook 'outline-minor-mode-hook
;; 	    (lambda ()
;; 	      (require 'outline-magic)
;; 	      (define-key outline-minor-mode-map (kbd "C-<tab>") 'outline-cycle))))

;; ;;; https://github.com/purcell/page-break-lines
;; (use-package page-break-lines
;;   :config
;;   (global-page-break-lines-mode +1))

;;; https://github.com/openscad/openscad/blob/master/contrib/scad-mode.el
(use-package scad-mode)
;;  :defer 2)

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

;; ;;; https://github.com/pashky/restclient.el
;; ;; see also: outline-magic
;; (use-package restclient
;;   :defer 2)

;; (use-package request
;;   :config
;;   (custom-set-variables '(request-storage-directory (format "%s/request" private-dir)))
;;   )

;; ;;; https://github.com/Fuco1/smartparens
;; ;; https://ebzzry.io/en/emacs-pairs/
;; ;; https://github.com/Fuco1/smartparens/wiki
;; (use-package smartparens
;;   :init
;;   (require 'smartparens-config)
;;   :config
;;   (progn (show-smartparens-global-mode t))
;;   (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
;;   (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
;;   )

;; ;;; https://github.com/emacs-berlin/syntactic-close
;; (use-package syntactic-close
;;   ;; :bind
;;   :config
;;   (global-set-key (kbd "C-]") 'syntactic-close)
;;   )

(use-package ssh-config-mode)

;; ;;; https://github.com/cjohansson/emacs-ssh-deploy
;; (use-package ssh-deploy)

;; ;;; https://github.com/aspiers/smooth-scrolling/
;; (use-package smooth-scrolling
;;   :init
;;   (smooth-scrolling-mode 1)
;;   :config
;;   (setq smooth-scroll-margin 2))

;;; https://github.com/io12/good-scroll.el
(use-package good-scroll
  :init
  (good-scroll-mode 1))


;;; https://github.com/holomorph/systemd-mode
(use-package systemd
  :hook (systemd-mode . company-mode))

;; ;;; https://github.com/saf-dmitry/taskpaper-mode
;; ;; taskpaper-mode - Major mode for working with TaskPaper files
;; (use-package taskpaper-mode
;;   ;; (add-to-list 'auto-mode-alist '("\\.todo\\'" . taskpaper-mode))
;;   )

;; ;;; https://github.com/davidshepherd7/terminal-here
;; (use-package terminal-here
;;   :config
;;   (cond
;;    ((eq system-type 'darwin)
;;     ;; this works on macOS local for iTerm
;;     (setq terminal-here-terminal-command (list "open" "-a" "iTerm.app" "."))
;;     ;; ;; this works on tramp session to a ubuntu remote
;;     ;; (setq terminal-here-terminal-command (list "/Users/dpc/bin/macos/emacs_term_here_iterm_ssh.py"))
;;     ))
;;   :bind
;;   (("C-<f5>" .  #'terminal-here-launch)
;;    ;; ("C-<f6>" .  #'terminal-here-project-launch)
;;    )
;;   )

;; https://github.com/koekeishiya/khd/issues/73#issuecomment-298112103
;; <shortcut> : osascript -e 'tell application "iTerm2" to create window with default profile'
;; <shortcut> : osascript -e 'tell application "iTerm2" to create window with default profile command "vim"'

;;; https://github.com/randymorris/tramp-term.el -
;; Automatic setup of directory tracking in ssh sessions.
;;     M-x tramp-term
;; (select ssh host to login to, then in emacs -> from shell there)
;;     C-x C-f
(use-package tramp-term)

;;; https://github.com/dryman/toml-mode.el
(use-package toml-mode)

;; ;;; https://github.com/lujun9972/verify-url
;; (use-package verify-url)

;; (use-package undo-tree
;;   :config
;;   ;; Remember undo history
;;   (setq
;;    undo-tree-auto-save-history nil
;;    undo-tree-history-directory-alist `(("." . ,(concat temp-dir "/undo/"))))
;;   (global-undo-tree-mode 1))

;;; https://wakatime.com/emacs
;;     pip3 install --user wakatime
(use-package wakatime-mode
  :diminish wakatime-mode
  :config
  ;; (setq wakatime-api-key "...") ;; moved to ~/.wakatime.cfg
  (global-wakatime-mode +1)
  )

;;; https://github.com/justbur/emacs-which-key
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode))

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
  ;; :config
  ;; (add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
  ;; (add-hook 'yaml-mode-hook #'ansible-doc-mode)
  )

;; ;;; https://github.com/joaotavora/yasnippet
;; (use-package yasnippet
;;   :defer 2
;;   :diminish yas-minor-mode
;;   :config
;;   (yas-global-mode 1))

;; ;; yasnippet org-mode conflict
;; ;;; https://orgmode.org/manual/Conflicts.html#Conflicts

;; (defun yas/org-very-safe-expand ()
;;   (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (make-variable-buffer-local 'yas/trigger-key)
;;             (setq yas/trigger-key [tab])
;;             (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
;;             (define-key yas/keymap [tab] 'yas/next-field)))

;; ;;; https://github.com/AndreaCrotti/yasnippet-snippets
;; (use-package yasnippet-snippets
;;   :defer 2
;;   )

;; ;;; https://github.com/sei40kr/gitignore-snippets
;; (use-package gitignore-snippets
;;   :after (yasnippet)
;;   :config
;;   (gitignore-snippets-init))


(provide 'base-extensions)

;;; base-extensions.el ends here
