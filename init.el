(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
;; Configure use-package to use straight.el by default
(use-package straight
             :custom (straight-use-package-by-default t))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load-file custom-file)

(defconst *is-a-mac* (eq system-type 'darwin))

(defconst *is-m1-laptop* (equal system-name "macbook-m1.lan"))
(if *is-m1-laptop* (message "System appears to be an M1 Macbook"))

;; (defvar eric-custom/default-font-size 130)
;; (set-face-attribute 'default nil :font "Fira Code" :height 132)
;; (if *is-a-mac*
;;     (set-face-attribute 'default nil :font "Fira Code" :height 140))

(use-package ligature
  :config
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(setq byte-compile-warnings '(cl-functions))

(scroll-bar-mode -1)      ; Disable visible scrollbar
  (tool-bar-mode -1)         ; Disable the toolbar
  (tooltip-mode -1)            ; Disable tooltips
  (set-fringe-mode 10)      ; Give some breathing room
  (menu-bar-mode 0)      ; Disable the menu bar

  (column-number-mode)
  (global-display-line-numbers-mode t) ;; Display line numbers globally

  ;; If we are in Emacs 29 or higher, use
  ;; precision scrolling mode for smooth scrolling
  (if (not (version< emacs-version "29.0"))
    (pixel-scroll-precision-mode 1))

  ;; Disable line numbering in these modes:
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  vterm-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  elpher-mode-hook
                  dired-mode-hook
                  markdown-mode-hook
                  ement-mode-hook
                  eat-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer

;; Deal with  TLS1.3 Bug that seems to affect Melpa?
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil)

(use-package lin
    :straight '(lin-mode :type git :host github :repo "protesilaos/lin")
    :bind
    ("C-c l" . lin-mode))

;;(set-face-attribute 'default nil :font "DinaRemasterII" :weight 'regular :height 170)

(defun eg/set-default-font-height ()
  (interactive)
  (let ((num (read-number "Enter new font height:  ")))
    (set-face-attribute 'default nil :height num)))

(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

(global-set-key (kbd "M-C-8") (lambda () (interactive) (sanityinc/adjust-opacity nil -2)))
(global-set-key (kbd "M-C-9") (lambda () (interactive) (sanityinc/adjust-opacity nil 2)))
(global-set-key (kbd "M-C-7") (lambda () (interactive) (modify-frame-parameters nil `((alpha . 100)))))

(use-package unicode-fonts
   :ensure t
   :config
   (unicode-fonts-setup))

(set-fontset-font t 'symbol "Noto Color Emoji" nil)
(set-fontset-font t 'symbol "Symbola" nil 'append)

(if *is-a-mac*
  (progn
    (setenv "GUILE_TLS_CERTIFICATE_DIRECTORY" "/opt/homebrew/etc/gnutls/")
    (setenv "GUILE_LOAD_PATH" "/opt/homebrew/share/guile/site/3.0")
    (setenv "GUILE_LOAD_COMPILED_PATH" "/opt/homebrew/lib/guile/3.0/site-ccache")
    (setenv "GUILE_SYSTEM_EXTENSIONS_PATH" "/opt/homebrew/lib/guile/3.0/extensions")))

(setq dired-listing-switches "-la")

(use-package all-the-icons-dired
  :hook ((dired-mode . all-the-icons-dired-mode)
         ;(dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode)))

(set-default-coding-systems 'utf-8)     ; Default to utf-8 encoding
(prefer-coding-system       'utf-8)     ; Add utf-8 at the front for automatic detection.
(set-terminal-coding-system 'utf-8)     ; Set coding system of terminal output
(set-keyboard-coding-system 'utf-8)     ; Set coding system for keyboard input on TERMINAL
(set-language-environment "English")    ; Set up multilingual environment

(defun eg/is-current-buffer (buff)
  "Respond true if the given buffer is the current buffer"
  (eq buff (current-buffer)))

(defun eg/get-all-non-current-buffers ()
  "Return a list of all current buffers aside from the current one"
  (seq-filter '(lambda (buff)
                 (not (eg/is-current-buffer buff)))
              (buffer-list)))

(defun kill-other-buffers ()
  "Kill all open buffers aside from the current one"
  (interactive)
  (mapcar 'kill-buffer (eg/get-all-non-current-buffers))
  (delete-other-windows))

(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))

(defun eg/vterm-mode-hook ()
  (define-key vterm-mode-map (kbd "C-<left>") 'windmove-left)
  (define-key vterm-mode-map (kbd "C-<right>") 'windmove-right)
  (define-key vterm-mode-map (kbd "C-<up>") 'windmove-up)
  (define-key vterm-mode-map (kbd "C-<down>") 'windmove-down))
(use-package vterm
  :hook
  (vterm-mode . eg/vterm-mode-hook))

(use-package dwim-shell-command
  :ensure t
  :bind (([remap shell-command] . dwim-shell-command)
         :map dired-mode-map
         ([remap dired-do-async-shell-command] . dwim-shell-command)
         ([remap dired-do-shell-command] . dwim-shell-command)
         ([remap dired-smart-shell-command] . dwim-shell-command))
  :config
  )

(straight-use-package
 '(eat :type git
       :host codeberg
       :repo "akib/emacs-eat"
       :files ("*.el" ("term" "term/*.el") "*.texi"
               "*.ti" ("terminfo/e" "terminfo/e/*")
               ("terminfo/65" "terminfo/65/*")
               ("integration" "integration/*")
               (:exclude ".dir-locals.el" "*-tests.el"))))

;; For `eat-eshell-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-mode)

;; For `eat-eshell-visual-command-mode'.
(add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

(use-package xref
    :straight t)

  (use-package project
    :straight t)

  (use-package eldoc
    :straight t)

(use-package svg-lib
             :straight '(svg-lib :type git :host github :repo "rougier/svg-lib"))

(use-package doom-themes
        ;;:init (load-theme 'doom-challenger-deep t)
         )

(use-package nano-theme
  :ensure nil
  :straight '(nano-theme :type git :host github :repo "rougier/nano-theme")
  :custom (nano-fonts-use t)
  :config(nano-mode)
  (load-theme 'nano-light t)
  (setq-default cursor-type 'box)
  (set-background-color "#FFFCF0"))

(defun eg/after-theme-load (_theme &rest args)
  (message "eg/after-theme-load!")
  (setq-default cursor-type 'box)
(advice-add 'load-theme :after 'eg/after-theme-load))

(nano-mode)
(load-theme 'nano-light t)
(setq-default cursor-type 'box)

(use-package flexoki-themes
  :straight '(flexoki-themes :type git :host github :repo "crmsnbleyd/flexoki-emacs-theme"))

(use-package nano-modeline
  :straight '(nano-modeline :type git :host github :repo "rougier/nano-modeline")
  :config (nano-modeline-text-mode t))

(add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
(add-hook 'text-mode-hook            #'nano-modeline-text-mode)
(add-hook 'org-mode-hook             #'nano-modeline-org-mode)
(add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
(add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
(add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
(add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
(add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
(add-hook 'term-mode-hook            #'nano-modeline-term-mode)
(add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
(add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
(add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
(add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)

(defun my/thin-modeline ()
  "Transform the modeline in a thin faded line"
  (setq mode-line-format (list ""))
  (setq-default mode-line-format (list ""))
  (set-face-attribute 'mode-line nil
                      :box nil
                      :inherit nil
                      :foreground (face-background 'nano-subtle)
                      :background (face-background 'nano-subtle)
                      :height 0.1)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil
                      :inherit nil
                      :foreground (face-background 'nano-subtle)
                      :background (face-background 'nano-subtle)
                      :height 0.1))

(add-hook 'prog-mode-hook #'my/thin-modeline)
(add-hook 'fundamental-mode-hook #'my/thin-modeline)
(add-hook 'text-mode-hook #'my/thin-modeline)
(add-hook 'special-mode-hook #'my/thin-modeline)
(add-hook 'prog-mode-hook            #'my/thin-modeline)
(add-hook 'text-mode-hook            #'my/thin-modeline)
(add-hook 'org-mode-hook             #'my/thin-modeline)
(add-hook 'pdf-view-mode-hook        #'my/thin-modeline)
(add-hook 'mu4e-headers-mode-hook    #'my/thin-modeline)
(add-hook 'mu4e-view-mode-hook       #'my/thin-modeline)
(add-hook 'elfeed-show-mode-hook     #'my/thin-modeline)
(add-hook 'elfeed-search-mode-hook   #'my/thin-modeline)
(add-hook 'term-mode-hook            #'my/thin-modeline)
(add-hook 'xwidget-webkit-mode-hook  #'my/thin-modeline)
(add-hook 'messages-buffer-mode-hook #'my/thin-modeline)
(add-hook 'org-capture-mode-hook     #'my/thin-modeline)
(add-hook 'org-agenda-mode-hook      #'my/thin-modeline)

(defun my/minibuffer-header ()
  "Minibuffer header"
  
  (let ((depth (minibuffer-depth)))
    (concat
     (propertize (concat "  " (if (> depth 1)
                                   (format "Minibuffer (%d)" depth)
                                 "Minibuffer ")
                         "\n")
                 'face `(:inherit (nano-subtle nano-strong)
                         :box (:line-width (1 . 3)
                               :color ,(face-background 'nano-subtle)
                               :style flat)
                         :extend t)))))

(defun my/minibuffer-setup ()
    "Install a header line in the minibuffer via an overlay (and a hook)"
  
    (set-window-margins nil 0 0)
    (set-fringe-style '(0 . 0))
    (cursor-intangible-mode t)
    (face-remap-add-relative 'default
                             :inherit 'highlight)
   (let* ((overlay (make-overlay (+ (point-min) 0) (+ (point-min) 0)))
          (inhibit-read-only t))

      (save-excursion
        (goto-char (point-min))
        (insert (propertize
                 (concat (my/minibuffer-header)
                         (propertize "\n" 'face `(:height 0.33))
                         (propertize " "))
                 'cursor-intangible t
                 'read-only t
                 'field t
                 'rear-nonsticky t
                 'front-sticky t)))))


(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup)

(use-package posframe
  :straight '(posframe :type git :host github :repo "tumashu/posframe"))

(use-package savehist
  :config (savehist-mode 1))

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                   :includes (vertico-indexed
                              vertico-flat
                              vertico-grid
                              vertico-mouse
                              vertico-quick
                              vertico-buffer
                              vertico-repeat
                              vertico-reverse
                              vertico-directory
                              vertico-multiform
                              vertico-unobtrusive
                              ))
  :custom (vertico-cycle t)
  :config (vertico-mode))

(use-package vertico-posframe
  :straight '(vertico-posframe :type git :host github :repo "tumashu/vertico-posframe"))

(setq vertico-multiform-commands
      '((consult-line
         posframe
         (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
         (vertico-posframe-border-width . 10)
         ;; NOTE: This is useful when emacs is used in both in X and
         ;; terminal, for posframe do not work well in terminal, so
         ;; vertico-buffer-mode will be used as fallback at the
         ;; moment.
         (vertico-posframe-fallback-mode . vertico-buffer-mode))
        (t posframe)))
(vertico-multiform-mode 1)

(use-package corfu
  :ensure t
  :custom ((corfu-cycle t) (corfu-auto t))
  :config (global-corfu-mode))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-use-icons t)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package orderless
  :custom ((completion-styles '(orderless))))

(use-package consult
  :bind (("C-s" . consult-line)
            ("C-x b" . consult-buffer))
  :custom ((xref-show-xrefs-function #'consult-xref)
           (xref-show-definitions-function #'consult-xref)))

(use-package consult-dir)

(use-package marginalia
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :config (marginalia-mode))

(use-package all-the-icons)

;; (use-package hydra)

(use-package helpful
	     :custom
	     (counsel-describe-function-function #'helpful-callable)
	     (counsel-describe-variable-function #'helpful-variable)
	     :bind
	     ([remap describe-function] . helpful-function)
	     ([remap describe-command] . helpful-command)
	     ([remap describe-variable] . helpful-variable)
	     ([remap describe-key] . helpful-key))

(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook)
  :custom ((dashboard-banner-logo-title "")
	   (dashboard-startup-banner "~/.emacs.d/lamassu.png")
	   (dashboard-center-content t)
	   (dashboard-set-heading-icons t)
	   (dashboard-set-file-icons t)
	   (dashboard-set-footer nil)))

(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<up>") 'windmove-up)

(eval-after-load "term"
  '(progn
     (define-key term-raw-map (kbd "C-<left>") 'windmove-left)
     (define-key term-raw-map (kbd "C-<right>") 'windmove-right)
     (define-key term-raw-map (kbd "C-<up>") 'windmove-up)
     (define-key term-raw-map (kbd "C-<down>") 'windmove-down)))
(eval-after-load "vterm"
  '(progn
     (define-key term-raw-map (kbd "C-<left>") 'windmove-left)
     (define-key term-raw-map (kbd "C-<right>") 'windmove-right)
     (define-key term-raw-map (kbd "C-<up>") 'windmove-up)
     (define-key term-raw-map (kbd "C-<down>") 'windmove-down)))

(use-package dimmer
  :custom ((dimmer-fraction 0.45)
           ;; :both will dim background and foreground
           (dimmer-adjustment-mode :foreground)))
  :config (dimmer-mode t)

(defun split-and-follow-horizontally ()
  "Split a new window horizontally and put the
cursor into the new window"
  (interactive)
  (split-window-below)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 2") 'split-and-follow-horizontally)

(defun split-and-follow-vertically ()
  "Split a new window vertically and put the
cursor into the new window"
  (interactive)
  (split-window-right)
  (balance-windows)
  (other-window 1))
(global-set-key (kbd "C-x 3") 'split-and-follow-vertically)

(bind-keys
 ("s-=" . text-scale-increase)
 ("s-\-" . text-scale-decrease))

(use-package beacon
  :init (beacon-mode 1)
  :custom ((beacon-lighter "")
	   (beacon-size 20)))

(use-package fullframe)

(use-package magit
  :init(fullframe magit-status magit-mode-quit-window)
  :bind (("C-x g" . magit-status))
  :custom ((magit-diff-refine-hunk t)))

(use-package git-commit
  :hook ((git-commit-mode . goto-address-mode)))

(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package add-node-modules-path)

;; (setq ruby-deep-indent-paren nil)

(defun eg/ruby-mode-hook ()
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil)
  (company-mode))
  
(use-package enh-ruby-mode
  :hook
  (enh-ruby-mode . eg/ruby-mode-hook)
  (enh-ruby-mode . delete-selection-mode)
  (enh-ruby-mode . hs-minor-mode)
  :config (lambda ()
    (add-to-list 'hs-special-modes-alist
    `(ruby-mode
    ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
    ,(rx (or "}" "]" "end"))                       ; Block end
    ,(rx (or "#" "=begin"))                        ; Comment start
    ruby-forward-sexp nil))))
  


(use-package ruby-electric
  :hook
  (enh-ruby-mode . ruby-electric-mode))

(use-package rvm
  :config (rvm-use-default))

;; Add Ruby files to the auto-mode setup
(add-to-list 'auto-mode-alist
             '("\\.\\(?:cap\\|gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . enh-ruby-mode))
(add-to-list 'auto-mode-alist
             '("\\(?:Brewfile\\|Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . enh-ruby-mode))

(use-package rubocop
   :ensure t
   :init
   :hook
   (enh-ruby-mode . rubocop-mode)
   :diminish rubocop-mode)

;; (use-package rspec-mode
;; :config (setq rspec-use-rvm nil))
;; (add-to-list 'auto-mode-alist '(".spec\\.rb\\'" . rspec-mode))

(use-package robe
  :after (company)
  :hook (enh-ruby-mode . robe-mode)
  :config ((lambda ()
             (push 'company-robe company-backends)))
  )
(advice-add 'inf-ruby-console-auto :before #'rvm-activate-corresponding-ruby)

(use-package nvm
:straight '(nvm :type git :host github :repo "rejeep/nvm.el"))

;; Add NodeJS error format
;; (setq compilation-error-regexp-alist-alist
;;       (cons '(node "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
;;                          1 ;; file
;;                          2 ;; line
;;                          3 ;; column
;;                          )
;;             compilation-error-regexp-alist-alist))
;; (setq compilation-error-regexp-alist-alist
;;       (cons '(npm "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
;;                          1 ;; file
;;                          2 ;; line
;;                          3 ;; column
;;                          )
;;             compilation-error-regexp-alist-alist))
;; (setq compilation-error-regexp-alist-alist
;;       (cons '(npx "^[  ]+at \\(?:[^\(\n]+ \(\\)?\\([a-zA-Z\.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)\)?$"
;;                          1 ;; file
;;                          2 ;; line
;;                          3 ;; column
;;                          )
;;             compilation-error-regexp-alist-alist))
;; (setq compilation-error-regexp-alist
;;       (cons 'node compilation-error-regexp-alist))
;; (setq compilation-error-regexp-alist
;;       (cons 'npx compilation-error-regexp-alist))
;; (setq compilation-error-regexp-alist
;;       (cons 'npm compilation-error-regexp-alist))

(defun eg/js2-mode-hook ()
    (progn
      (setq mode-name "JS2")
      (add-node-modules-path)))
(use-package js2-mode
  :hook (js2-mode . eg/js2-mode-hook)
  :custom ((js-indent-level 2)))

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))

(defun eg/typescript-mode-hook ()
  (company-mode)
  (eglot-ensure)
  (add-node-modules-path))
;; Taken from (https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/)
(use-package typescript-mode
  :after tree-sitter
  :ensure t
  :hook
  (typescript-mode . eg/typescript-mode-hook))
(define-derived-mode typescriptreact-mode typescript-mode "TSX")
(add-to-list 'auto-mode-alist '("\\.tsx?\\'"  . typescriptreact-mode))
(add-to-list 'auto-mode-alist '("\\.ts?\\'"  . typescript-mode))

(defun eg/rjsx-mode-hook ()
        ;;(js2-minor-mode)
        (add-node-modules-path)
        (company-mode)
        (eglot-ensure)
        )
(use-package rjsx-mode
  :hook (rjsx-mode . eg/rjsx-mode-hook))

(add-to-list 'auto-mode-alist '("\\.jsx?$" . rjsx-mode))

(use-package prettier)

(use-package eslint-rc
  :hook ((js2-mode . eslint-rc-mode)
         (typescript-mode . eslint-rc-mode)
         (rjsx-mode . eslint-rc-mode)))

(use-package popwin
  :config (lambda ()
            (push "*xref*" popwin:special-display-config)))

(use-package json-mode)

(defun jq-this-file (jq-query)
  (interactive "sJQ Query: ")
  (save-excursion
    (mark-whole-buffer)
    (let ((cmd (concat "jq '" jq-query "'"))
          (output-buf (get-buffer-create "*jq*")))
      (shell-command-on-region (mark) (point) cmd output-buf)
      (switch-to-buffer-other-window output-buf))))

(use-package web-mode
:custom (web-mode-enable-engine-detection t))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb?\\'" . web-mode))

(use-package sass-mode)
(add-to-list 'auto-mode-alist '("\\.scss?\\'" . sass-mode))

(use-package markdown-mode)

(use-package grip-mode
  :ensure t
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(use-package yaml-mode)
(add-to-list 'auto-mode-alist '("\\.\\(yml\\|yaml\\)?\\'" . yaml-mode))

(add-hook 'lisp-mode-hook 'show-paren-mode)

(use-package geiser)
(use-package geiser-guile)

;; (defun eg/lsp-mode-setup ()
  ;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  ;;   (lsp-headerline-breadcrumb-mode)
  ;;   (lsp-deferred))
  ;; (use-package lsp-mode
  ;;   :commands (lsp lsp-deferred)
  ;;   :hook ((js2-mode . eg/lsp-mode-setup))
  ;;   :init
  ;;   (setq lsp-keymap-prefix "C-c l")
  ;;   :config
  ;;   (lsp-enable-which-key-integration t))
;; LSP ivy integration allows things like jumping
;; to definitions in a file from a list
;;(use-package lsp-ivy)

(use-package eglot
  :config
  ;; We define a custom eglot hover function to deal with
  ;; Solargraph's returning of null when hovering over an
  ;; empty area. See (https://github.com/joaotavora/eglot/issues/1019#issuecomment-1230546329)
  (defun eglot--format-markup (markup)
  "Format MARKUP according to LSP's spec."
  (if (plist-get markup :value)
      (pcase-let ((`(,string ,mode)
               (if (stringp markup) (list markup 'gfm-view-mode)
                 (list (plist-get markup :value)
                       (pcase (plist-get markup :kind)
                         ("markdown" 'gfm-view-mode)
                         ("plaintext" 'text-mode)
                         (_ major-mode))))))
    (with-temp-buffer
      (setq-local markdown-fontify-code-blocks-natively t)
      (insert string)
      (let ((inhibit-message t)
	    (message-log-max nil))
        (ignore-errors (delay-mode-hooks (funcall mode))))
      (font-lock-ensure)
      (string-trim (filter-buffer-substring (point-min) (point-max)))))
      "\n"))
  (add-to-list 'eglot-server-programs
   '((typescript-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
   '((typescriptreact-mode) "typescript-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs
  '(enh-ruby-mode "solargraph" "socket" "--port" :autoport))
  )

;; (defun eg/project-try-ts-config-json (dir)
;;   (when-let* ((found (locate-dominating-file-dir "tsconfig.json")))
;;     (cons 'eglot-project found)))
;; (add-hook 'project-find-functions 'eg/project-try-ts-config-json nil nil)
;; (add-to-list 'eglot-server-programs
;;              '((typescript-mode) "typescript-language-server" "--stdio"))

(use-package vue-mode)

(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))

(set-face-attribute 'show-paren-match nil :weight 'extra-bold :underline t)

(use-package drupal-mode)

(defun eg/php-mode-hook ()
  (setq indent-tabs-mode nil tab-width 2 c-basic-offset 2))
(add-hook 'php-mode-hook 'eg/php-mode-hook)

(use-package geben)

(use-package olivetti)

(use-package company
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package elpher
  :custom-face
  (fixed-width ((t :family "Fira Sans"))))

(setq org-agenda-files '("~/Sync/"))

(use-package org)
;; Basic Defaults
(setq-default org-ellipsis " …"              ; Nicer ellipsis
            org-tags-column 1              ; Tags next to header title
            org-hide-emphasis-markers t    ; Hide markers
            org-cycle-separator-lines 2    ; Number of empty lines between sections
            org-use-tag-inheritance nil    ; Tags ARE NOT inherited 
            org-use-property-inheritance t ; Properties ARE inherited
            org-indent-indentation-per-level 2 ; Indentation per level
            org-link-use-indirect-buffer-for-internals t ; Indirect buffer for internal links
            org-fontify-quote-and-verse-blocks t ; Specific face for quote and verse blocks
            org-return-follows-link nil    ; Follow links when hitting return
            org-image-actual-width nil     ; Resize image to window width
            org-indirect-buffer-display 'other-window ; Tab on a task expand it in a new window
            org-outline-path-complete-in-steps nil ; No steps in path display
            org-return-follows-link t) ;Self explanatory

(use-package org-modern
:hook (org-mode . org-modern-mode))

(defun eg/get-project-org-capture-file ()
  (concat (projectile-project-root) "todo.org"))
(defun eg/org-capture-get-line-number-string ()
  "Get the line number as a string from an org-capture session"
  )
(setq org-capture-templates
      '(("t" "Basic TODO" entry (file+headline "todo.org" "Basic Tasks")
         "* TODO %?\n %i\n %a")
        ("p" "Project TODO" entry (file+headline (lambda () (eg/get-project-org-capture-file)) "Project Tasks")
         "* TODO %?\n %t\n In file: [[file:%F::%(with-current-buffer (org-capture-get :original-file-nondirectory) (number-to-string (line-number-at-pos)))][%f]]\n\n")))

(setq-default org-src-fontify-natively t         ; Fontify code in code blocks.
            org-adapt-indentation nil          ; Adaptive indentation
            org-src-tab-acts-natively t        ; Tab acts as in source editing
            org-confirm-babel-evaluate nil     ; No confirmation before executing code
            org-edit-src-content-indentation 0 ; No relative indentation for code blocks
            org-fontify-whole-block-delimiter-line t) ; Fontify whole block

;; (setq eg/org-mode-font-family "Baskerville")
;; (add-hook
;;  'org-mode-hook
;;  (lambda ()
;;     (setq line-spacing 0.2) ;; Add more line padding for readability
;;     ;; We set the variable pitch here because we are using semi-quoted
;;     ;; for variables, which apparently does not work with use-package's
;;     ;; basic :custom-face capability
;;     (custom-set-faces `(variable-pitch ((t (:family ,eg/org-mode-font-family)))))
;;     (variable-pitch-mode 1) ;; All fonts with variable pitch.
;;     (text-scale-adjust 3) ;; Adjust text scale
;;     (mapc
;;      (lambda (face) ;; Other fonts with fixed-pitch.
;;        (set-face-attribute face nil :inherit 'fixed-pitch))
;;      (list 'org-code
;;            'org-link
;;            'org-block
;;            'org-table
;;            'org-verbatim
;;            'org-block-begin-line
;;            'org-block-end-line
;;            'org-meta-line
;;            'org-document-info-keyword))))
;; (message "Using macbook-m1.lan org font settings")

(use-package eshell-git-prompt
  :ensure t)

;; Custom eshell-git-prompt theme
(defun eshell-git-prompt-nano-powerline ()
  (let ((segment-separator "\xe0b0")
        (branch            "\xe0a0")
        (detached          "\x27a6")
        (cross             "\x2718")
        prefix dir git git-face)
    (setq prefix
      (if (eshell-get-variable "NODE_VIRTUAL_ENV")
        (propertize (concat ""
          (propertize " " 'display
            (svg-lib-icon "forest" nil :collection "material"
              :margin 0 :radius: 0 :stroke 0 :padding 0
              :foreground (face-background 'nano-popout-i)
              :background (face-background 'nano-salient-i)))
            ) 'face 'nano-salient-i)
        ""))
    (setq dir
          (propertize
           (concat
            " "
            (unless (eshell-git-prompt-exit-success-p)
              (concat cross " "))
            (eshell-git-prompt-powerline-dir)
            " ")
           'face 'nano-salient-i))
    (setq git
          (when (eshell-git-prompt--git-root-dir)
            (setq git-face
                  (if (eshell-git-prompt--collect-status)
                      'nano-popout-i
                    'nano-faded-i))
            (setq eshell-git-prompt-branch-name (eshell-git-prompt--branch-name))
            (propertize
             (concat " "
                     (-if-let (branch-name eshell-git-prompt-branch-name)
                         (concat branch " " branch-name)
                       (concat detached " "(eshell-git-prompt--commit-short-sha)))
                     " ")
             'face git-face)))
    (eshell-git-prompt---str-read-only
     (concat
      (if git
          (concat prefix dir
                  (with-face segment-separator
                    :foreground (face-background 'nano-salient-i)
                    :background (face-background git-face))
                  git
                  (with-face segment-separator
                    :foreground (face-background 'nano-popout-i)))
        (concat prefix dir
                (with-face segment-separator
                  :foreground (face-background 'nano-salient))))
      (propertize "$" 'invisible t) " "))))

(add-to-list 'eshell-git-prompt-themes '(nano-powerline eshell-git-prompt-nano-powerline eshell-git-prompt-powerline-regexp))

(defun eg/eshell-mode-hook ()
  (eshell-git-prompt-use-theme 'nano-powerline))
         
(add-hook 'eshell-mode-hook 'eg/eshell-mode-hook)

(use-package restclient)

;; Add an interactive to make a rest buffer
(defun rest-client ()
  (interactive)
  (let ((buff (get-buffer-create "*REST CLIENT*")))
    (with-current-buffer buff
      (switch-to-buffer buff)
      (restclient-mode))))

(use-package wgrep
  :straight '(wgrep :type git :host github :repo "mhayashi1120/Emacs-wgrep"))

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :config (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Retrieves the current file path without
;; the Projectile project's root at the beginning
;; (ie, a "project-relative" path of the current file)
(defun eg/project-filename ()
  (if buffer-file-name
      (substring
       buffer-file-name
       (length (projectile-project-root))
       nil)
    ""))

(defun eg/project-filename-to-clipboard ()
  "Place the project root relative path of
  the file of the current buffer into the clipboard"
  (interactive)
  (let ((path (eg/project-filename)))
    (with-temp-buffer
      (insert path)
      (clipboard-kill-region (point-min) (point-max)))))

(add-hook 'projectile-mode-hook (lambda ()(projectile-register-project-type 'deno '("deno.json")
  :project-file "deno.json"
  :run "deno task start")))

(defun eg/projectile-make-eleventy-type ()
  (projectile-register-project-type 'eleventy '(".eleventy.js")))
(add-hook 'projectile-mode-hook 'eg/projectile-make-eleventy-type)

(defun eg/compilation-mode-q ()
(local-set-key (kbd "q") 'kill-buffer-and-window))

(add-hook 'compilation-mode-hook #'eg/compilation-mode-q)

(use-package flymake
:straight nil
:custom
(flymake-fringe-indicator-position nil)
:hook
(prog-mode . flymake-mode))

(use-package flymake-eslint)
(use-package flymake-ruby
  :hook (enh-ruby-mode . flymake-ruby-load))
(add-hook 'js2-mode (lambda () (flymake-eslint-enable)))
(add-hook 'typescript-mode (lambda () (flymake-eslint-enable)))
(add-hook 'rjsx-mode (lambda () (flymake-eslint-enable)))

(use-package tree-sitter
  :hook
  (js2-mode . tree-sitter-hl-mode)
  (typescript-mode . tree-sitter-hl-mode)
  (typescriptreact-mode . tree-sitter-hl-mode)
  (enh-ruby-mode . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (add-to-list
    'tree-sitter-major-mode-language-alist
    '(typescriptreact-mode . tsx))
  (add-to-list
    'tree-sitter-major-mode-language-alist
    '(enh-ruby-mode . ruby)))

(use-package yasnippet)
(yas-global-mode 1)
(setq org-src-tab-acts-natively nil)

(use-package yasnippet-snippets)

()

(use-package idp
  :bind (("C-c i" . idp-main-transient))
  :straight '(idp :type git :host github :repo "18F/idp-emacs"))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(defun eg/set-node-env-variables (variables-alist)
  (let ((node-variable-names '("NODE_VIRTUAL_ENV" "_OLD_NODE_VIRTUAL_PATH" "_OLD_NODE_PATH" "NODE_PATH" "_OLD_NPM_CONFIG_PREFIX" "NPM_CONFIG_PREFIX")))
    (mapc
     (lambda (variable-name)
       (let ((found-variable (assoc-default variable-name variables-alist)))
         (if found-variable
             (progn
               (eshell-set-variable variable-name found-variable)))))
     node-variable-names))
  (eshell-set-variable "PATH" (assoc-default "PATH" variables-alist)))
  
(defun nodeenv-set-environment (path)
  (setq eshell-modify-global-environment t)
  (let* ((cmd (format "zsh -c \"source %s/bin/activate && printenv\"" path))
        (cmd-output (shell-command-to-string cmd))
        (lines (s-split "\n" cmd-output))
        (variables-alist (mapcar
                          (lambda (line)
                            (let ((split (s-split "=" line)))
                              (cons (car split) (cadr split))))
                          lines))
        (current-env (mapcar #'copy-sequence process-environment)))
    (eg/set-node-env-variables variables-alist)
    (setq eg/nodeenv--stashed-current-env current-env))
  (setq eshell-modify-global-environment nil))

(defalias 'eshell/nse 'nodeenv-set-environment)

(defun eg/run-initial-weather-gov-setup ()
  (let* ((commands '(
       "git clone git@github-tts:weather-gov/weather.gov"
       "cd weather.gov"
       "nodeenv --prebuilt --node=lts .nodeenv"
       "npm install"
       "cp weather.gov/web/sites/example.settings.dev.php web/sites/settings.dev.php"))
       (command-string (string-join commands " && ")))
    (shell-command command-string)
    (eshell-command "cd weather.gov && nse .nodeenv"))
)
(defalias 'eshell/wg 'eg/run-initial-weather-gov-setup t)

(load "~/.emacs.d/eric-functions.el")

(defcustom eg/irc-password nil "Default password to use for irc" :group 'eg)
;; More custom vars here
(load custom-file)

(defun eg/get-irc-password ()
  "Set the custom irc pass variable and add it to the
rcirc authinfo list for Freenode"
  (interactive)
  (customize-save-variable 'eg/irc-password (read-passwd "Enter Libera pass: "))
  (customize-save-variable 'rcirc-authinfo `(("libera" nickserv "darth-cheney" ,eg/irc-password)))
  (customize-save-variable 'rcirc-default-nick "darth-cheney"))

(if (or (not (boundp 'eg/irc-password)) (equal eg/irc-password nil))
    (progn
      (call-interactively 'eg/get-irc-password)))

(if *is-m1-laptop*
  (progn
    (set-face-attribute 'default nil :family "iA Writer Quattro V" :height 170 :weight 'light)
    (message "Set M1 font family!")))

(set-background-color "#FFFCF0")
