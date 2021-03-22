;; My Base Emacs Configuration

;; Preamble ------------------------------------------------------------
;; Put custom defined variables in a separate
;; file, and load that file if it's there
(setq custom-file (concat user-emacs-directory "/custom.el"))
(load-file custom-file)

;; My default fontsize
(defvar eric-custom/default-font-size 120)

;; Basic UI ------------------------------------------------------------
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

(column-number-mode)
(global-display-line-numbers-mode t) ;; Display line numbers globally

;; Disable line numbering in these modes:
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Package Management Config -------------------------------------------
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)

;; Load packages if they aren't currenlty present
(unless package-archive-contents
  (package-refresh-contents))

;; Init use-package for easy
;; package management and configuration
(unless (package-installed-p 'use-package)
   (package-refresh-contents)
   (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; (column-number-mode)
;; (global-display-line-numbers-mode t)
;; (use-package command-log-mode) ;; For logging keypresses

;; Custom Commands
;; ---------------------------------------------------------------------
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

;; Ivy Configuration ---------------------------------------------------
(use-package ivy
	     :diminish
	     :bind (("C-s" . swiper)
		    :map ivy-minibuffer-map
		    ("TAB" . ivy-alt-done)
		    ("C-k" . ivy-next-line)
		    ("C-j" . ivy-previous-line))
	     :config
	     (ivy-mode 1))

(use-package ivy-rich
	     :init
	     (ivy-rich-mode 1))

;; All the icons.
;; Note that you will need to run
;; M-x all-the-icons-install-fonts
;; interactively when you first load a fresh config
(use-package all-the-icons)

;; Hydra Config ----------------------------------------------------------
(use-package hydra)


;; Modeline Config -------------------------------------------------------
;; DOOM Modeline
;; This gives us a really good looking modeline
;; by default.
(use-package doom-modeline
	    :init (doom-modeline-mode 1)
	    :custom ((doom-modeline-height 40)))

;; Basic Themes Config ----------------------------------------------------

;; Use the DOOM Themes, which are all
;; pretty nice
(use-package doom-themes
	     :init (load-theme 'doom-challenger-deep t))

;; Counsel Config ---------------------------------------------------------

(use-package counsel
	     :bind (("M-x" . counsel-M-x)
		   ("C-x b" . counsel-switch-buffer)
		   ("C-x C-f" . counsel-find-file)
		   :map minibuffer-local-map
		   ("C-r" . 'counsel-minibuffer-history)))

;; Also init which-key, which helps with
;; documentation and provides the delay before
;; showing futher bindings
(use-package which-key
	     :init (which-key-mode)
	     :diminish which-key-mode
	     :config
	     (setq which-key-idle-delay 1))

;; Helpful (package) Config ------------------------------------------------
;; The package 'helpful' integrates better help and
;; interactive documentation. Especially powerful when
;; used with Ivy/Counsel
(use-package helpful
	     :custom
	     (counsel-describe-function-function #'helpful-callable)
	     (counsel-describe-variable-function #'helpful-variable)
	     :bind
	     ([remap describe-function] . counsel-describe-function)
	     ([remap describe-command] . helpful-command)
	     ([remap describe-variable] . counsel-describe-variable)
	     ([remap describe-key] . helpful-key))

;; Dashboard Config ---------------------------------------------------------
;; Dashboard is the opening screen that you see when Emacs first opens.
;; Using the dashboard package, we provide a customized version of that
;; page which shows things like the org agenda, recent files, a custom
;; image and buttons, etc
(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook)
  :custom ((dashboard-banner-logo-title "")
	   (dashboard-startup-banner "~/.emacs.d/lamassu.png")
	   (dashboard-center-content t)
	   (dashboard-set-heading-icons t)
	   (dashboard-set-file-icons t)
	   (dashboard-set-footer nil)))

;; General Keybindings and Window Movement -----------------------------

;; Basic navigation around different windows
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<up>") 'windmove-up)

;; Dim the other (inactive) buffer windows
(use-package dimmer
  ;;:diminish dimmer-mode
  ;;:disabled
  :custom ((dimmer-fraction 0.25)
           ;; :both will dim background and foreground
           (dimmer-adjustment-mode :foreground)))
  :config (dimmer-mode t)

;; Ensure that when we split new windows, the cursor
;; follows to the new window
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


;; Deal with windmove controls in terminal char mode
(eval-after-load "term"
  '(progn
     (define-key term-raw-map (kbd "C-<left>") 'windmove-left)
     (define-key term-raw-map (kbd "C-<right>") 'windmove-right)
     (define-key term-raw-map (kbd "C-<up>") 'windmove-up)
     (define-key term-raw-map (kbd "C-<down>") 'windmove-down)))

;; Cursor and Pointer Config -------------------------------------------

;; Use Beacon to light the way
(use-package beacon
  :init (beacon-mode 1)
  :custom ((beacon-lighter "")
	   (beacon-size 20)))

;; Comprehensive UI Config ---------------------------------------------

;; Some Initial Defaults (leftover from old config)
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

;; Opacity related settings
;; In the end, the following keybindings will be
;; available:
;;     M-C-8 -- Decrease opacity
;;     M-C-9 -- Increase opacity
;;     M-C-7 -- Set opacity to 100
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

;; Magit Config --------------------------------------------------------
;; Ensure magit shows in full frames (much better)
(use-package fullframe)

(use-package magit
  :init(fullframe magit-status magit-mode-quit-window)
  :bind (("C-x g" . magit-status))
  :custom ((magit-diff-refine-hunk t)))

(use-package git-commit
  :hook ((git-commit-mode . goto-address-mode)))

;; Completion Config ---------------------------------------------------

;; Paren and bracket autocompletion
;; (See JS2 setup for example.
;;  key is to use electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Custom Functions and Commands ---------------------------------------

;; Add command for killing all buffers
(defun kill-all-buffers ()
  "Kill all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (delete-other-windows))


;; Misc Settings ------------------------------------------------------
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Deal with  TLS1.3 Bug that seems to affect Melpa?
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")


;; Language Specific Configs ------------------------------------------

;; |----------------------|
;; | Javascript           |
;; |----------------------|
(defun eg/js2-mode-hook ()
  (progn
    (setq mode-name "JS2")))
(use-package js2-mode
  :hook (js2-mode . eg/js2-mode-hook)
  :custom ((js-indent-level 4)))

(add-to-list 'auto-mode-alist '("\\.\\(js\\|es6\\)\\(\\.erb\\)?\\'" . js2-mode))
(use-package json-mode)
(use-package prettier-js)


;; |----------------------|
;; | HTML / Web           |
;; |----------------------|
(use-package web-mode
  :config (
                                        ;(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
           ))

;; LSP Config ----------------------------------------------------------
(defun eg/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  (lsp-deferred))
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((js2-mode . eg/lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

;; LSP ivy integration allows things like jumping
;; to definitions in a file from a list
(use-package lsp-ivy)


;; DAP Config ----------------------------------------------------------
(use-package dap-mode
  :config
  (require 'dap-node)
  (dap-node-setup)
  (require 'dap-firefox)
  (require 'dap-chrome))

;; Company Mode Config -------------------------------------------------
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Org Mode Config -----------------------------------------------------
(use-package org)

;; Elisp and Generic Lisp Config ---------------------------------------
(add-hook 'lisp-mode-hook 'show-paren-mode)

;; Slime config --------------------------------------------------------
(defun eg/setup-slime-keybindings ()
  "For use in slime-repl-mode-hook for
fixing clashing windmove keybindings"
  (define-key slime-repl-mode-map (kbd "C-<down>") 'windmove-down)
  (define-key slime-repl-mode-map (kbd "C-<up>") 'windmove-up)
  (define-key slime-repl-mode-map (kbd "C-<right>") 'windmove-right)
  (define-key slime-repl-mode-map (kbd "C-<left>") 'windmove-left)
  (define-key slime-repl-mode-map (kbd "S-C-<up>") 'slime-repl-previous-input)
  (define-key slime-repl-mode-map (kbd "S-C-<down>") 'slime-repl-next-input))

(use-package slime
  :custom (inferior-lisp-program "sbcl")
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el")))

;;(add-hook 'slime-mode-hook #'eg/setup-slime-keybindings)
(add-hook 'slime-repl-mode-hook #'eg/setup-slime-keybindings)

;; Add auto-mode for lisp files
(add-to-list 'auto-mode-alist '("\\.cl$" . common-lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))


;; Scheme Config (Geiser) ----------------------------------------------
(use-package geiser)

;; Dired Mode Config ---------------------------------------------------
(define-key dired-mode-map [mouse-2] 'dired-mouse-find-file)

;; Eshell
(use-package eshell-git-prompt)
(use-package eshell
  :config
  (eshell-git-prompt-use-theme 'powerline))

;; IRC -----------------------------------------------------------------
  (defcustom eg/irc-password nil "Default password to use for IRC connections")
  (setq rcirc-default-nick "darth-cheney")
  (defun eg/get-irc-password (passwd)
    "Set the custom irc pass variable and add it to the
rcirc authinfo list for Freenode"
    (interactive "sEnter your Freenode Pass: ")
    (setq eg/irc-password passwd)
    (setq rcirc-authinfo '(("freenode" nickserv "darth-cheney" eg/irc-password))))

(if (not eg/irc-password) (call-interactively 'eg/get-irc-password))
(put 'erase-buffer 'disabled nil)

;; Org Mode Config -----------------------------------------------------
(defun eg/org-mode-setup ())

(use-package org
  :hook ((org-mode . eg/org-mode-setup)))

(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-skip-unavailable-files t)

;; Org Journal Config --------------------------------------------------
(use-package org-journal)

;; Command Log Mode ----------------------------------------------------
;; for debugging entered key commands
(use-package command-log-mode)

;; Snippets Config -----------------------------------------------------
(use-package yasnippet)
(yas-global-mode 1)
(use-package yasnippet-snippets)

;; Treemacs Mode Config ------------------------------------------------
(use-package treemacs)
(global-set-key (kbd "C-t") 'treemacs)

;; Olivetti Mode Config ------------------------------------------------
(use-package olivetti)

;; Org-Roam Config -----------------------------------------------------
(use-package org-roam
  :config
  (make-directory "~/Documents/org-roam" t)
  :custom
  (org-roam-directory "~/Documents/org-roam")
  :hook
  ((after-init . org-roam-mode)))

;; Projectile Config ---------------------------------------------------
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)f
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/projects")
    (setq projectile-project-search-path '("~/projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Additional Custom Functions -----------------------------------------
(load "~/.emacs.d/eric-functions.el")
