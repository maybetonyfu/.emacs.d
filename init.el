(menu-bar-mode 0)
;; OS X related settings
(when (eq system-type 'darwin)
  (menu-bar-mode 1)
  (setf
   mac-command-modifier 'meta
   mac-option-modifier 'super
   mac-control-modifier 'control
   mac-function-modifier 'hyper)

   ;; Make sure zsh exe paths are sourced
   (use-package exec-path-from-shell
     :ensure t
     :init
     (exec-path-from-shell-initialize)))

(put 'temporary-file-directory 'standard-value
     (list temporary-file-directory))

(setf custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;; Set up treemacs
(use-package treemacs
  :ensure t
  :init (setf treemacs-no-png-images nil)
  :bind (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; Display column number in mode-line
(column-number-mode)
(global-visual-line-mode)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

(use-package paren
  :ensure t
  :init (setf show-paren-delay 0.25)
        (show-paren-mode))

(setf require-final-newline t)
(setf frame-inhibit-implied-resize t)

(setf auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setf backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setf create-lockfiles nil)

(scroll-bar-mode 0)

(if (display-graphic-p)
  (tool-bar-mode 0)
  (menu-bar-mode 0))

(defalias 'yes-or-no #'y-or-n-p)

(setf
 pixel-scroll-precision-mode t
 window-resize-pixelwise t
 frame-resize-pixelwise t)

(save-place-mode t)

(winner-mode 1)

(use-package spacious-padding
  :ensure t
  :init (setf spacious-padding-widths
         '(:right-divider-width 1
                                :fringe-width 20))
        (spacious-padding-mode))

(use-package dimmer
  :ensure t
  :init (progn
         (dimmer-configure-magit)
         (dimmer-configure-which-key)
         (dimmer-mode t)))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package dashboard
  :ensure t
  :config (dashboard-setup-startup-hook)
  :init (setf dashboard-startup-banner 'logo
              dashboard-display-icons-p t
              dashboard-icon-type 'nerd-icons
              dashboard-set-heading-icons t
              dashboard-set-file-icons t
              dashboard-items '((recents . 5)
                                (bookmarks . 5)
                                (projects . 5))))

;; PERSIST history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)


(setf minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(setf enable-recursive-minibuffers t))

;; UI Related Packages
(use-package helpful
  :ensure t
  :bind (
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

(use-package which-key
  :ensure nil
  :config (which-key-mode))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))


;; Completion Libraries
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

(use-package orderless
  :ensure t
  :config
  (setf completion-styles '(orderless basic))
  (setf completion-category-defaults nil)
  (setf completion-category-overrride '((file (styles . (partial-completion))))))

(use-package corfu
  :ensure t
  :hook (after-init . global-corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setf tab-always-indent 'complete
        corfu-preview-current nil
        corfu-min-width 20
        corfu-popupinfo-delay '(1.25 . 0.5))
  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;; Writing Modes: markdown, org and LaTeX
(use-package markdown-mode
  :ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (python . t)))

(setf org-babel-python-command "python3")

(use-package latex-preview-pane
  :ensure nil
  :load-path "~/.emacs.d/latex-preview-pane/")

(use-package auctex
  :ensure t
  :hook (LaTeX-mode . TeX-fold-mode))

(use-package howm
  :init
  (require 'howm-markdown)
  (setq howm-directory "~/Nextcloud/Notes/Howm")
  (setq howm-menu-expiry-hours 1)
  (setq howm-menu-refresh-after-save nil)
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.md")

  ;; Using ripgrep
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil))

;; Programming Modes
(use-package haskell-mode
  :ensure t)

(use-package typescript-mode
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package python-mode
  :ensure t)

(use-package web-mode
  :ensure t
  :mode (
    ("\\.svelte\\'" . web-mode)
  ))

(use-package go-mode
  :ensure t)

(setf inferior-lisp-program "sbcl")

(add-hook 'prog-mode-hook
          (lambda ()
            (progn
              (display-line-numbers-mode) ; Show line number
              (make-local-variable 'show-trailing-whitespace) ; Show trailing whitespace
              (setf show-trailing-whitespace t)
               )))


(use-package mise
  :ensure t
  :config (global-mise-mode))

;; (add-hook 'python-mode-hook 'eglot-ensure)
;; (add-hook 'go-mode-hook 'eglot-ensure)
;; (add-hook 'haskell-mode-hook 'eglot-ensure)
(use-package eglot
  :config (add-to-list 'eglot-server-programs
                       '(haskell-mode . ("haskell-language-server-wrapper" "lsp"))
                       '(python-mode . ("basedpyright-langserver" "--stdio"))))

(use-package magit
  :ensure t)

(use-package tldr
  :ensure t)

(use-package eat
  :ensure t)

(use-package verb
  :ensure t)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(use-package gptel
  :ensure t
  :config (global-set-key (kbd "C-c 1") 'gptel-send))

(use-package disproject
  ;; Replace `project-prefix-map' with `disproject-dispatch'.
  :bind (:map ctl-x-map ("p" . disproject-dispatch)))

;; Load Personal Script
(load-file (expand-file-name "tf.el" user-emacs-directory))
;; (load-file "~/.emacs.d/password-generator/password-generator.el")

;; Global Key Binds
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-a") 'view-mode)

(global-set-key [remap cycle-spacing] 'just-one-space)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(global-set-key [remap zap-to-char] 'tf-move-to-char)
(global-set-key (kbd "C-`") 'tf-term)
(global-set-key (kbd "M-]") 'tf-move-line-down)
(global-set-key (kbd "M-<down>") 'tf-move-line-down)
(global-set-key (kbd "M-[") 'tf-move-line-up)
(global-set-key (kbd "M-<up>") 'tf-move-line-up)
(global-set-key (kbd "C-g") 'tf-keyboard-quit-dwim)

;; Personal Command Bindings
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'set-goal-column 'disabled nil)
