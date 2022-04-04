(require 'package)
(require 'view)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(smartparens-global-mode t)
(require 'smartparens-config)

(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq default-frame-alist '((width . 135) (height . 40)))
(set-window-scroll-bars (minibuffer-window) nil nil)

(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'window-swap-states)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f1>") 'shell)
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

(defun my-delete-shell-window ()
  (interactive)
  (delete-windows-on "*shell*" nil))

(defun my-delete-shell-window-key ()
  (local-set-key (kbd "<f1>") 'my-delete-shell-window))

(add-hook 'shell-mode-hook 'my-delete-shell-window-key)

(tool-bar-mode -1)
(menu-bar-mode +1)
(selectrum-mode +1)
(setq completion-styles '(orderless))
(savehist-mode)

(whole-line-or-region-global-mode +1)
(global-tab-line-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes t)
 '(dired-dwim-target 'dired-dwim-target-next)
 '(display-line-numbers-width 3)
 '(fci-rule-color "#4E4E4E")
 '(haskell-compile-stack-build-command "stack build --fast")
 '(haskell-compiler-type 'stack)
 '(highlight-tail-colors
   ((("#dce6e0" "color-22" "green")
     . 0)
    (("#dce8ed" "color-30" "cyan")
     . 20)))
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(jdee-db-active-breakpoint-face-colors (cons "#D0D0E3" "#009B7C"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#D0D0E3" "#005F00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#D0D0E3" "#4E4E4E"))
 '(make-backup-files nil)
 '(next-screen-context-lines 15)
 '(objed-cursor-color "#D70000")
 '(package-selected-packages
   '(modus-themes orderless whole-line-or-region selectrum-prescient selectrum smartparens dumb-jump haskell-mode doom-themes))
 '(rustic-ansi-faces
   ["#F5F5F9" "#D70000" "#005F00" "#AF8700" "#1F55A0" "#AF005F" "#007687" "#0F1019"])
 '(scroll-conservatively 100)
 '(sp-base-key-bindings 'sp)
 '(standard-indent 2)
 '(tab-always-indent t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(whitespace-line ((t nil))))

(when (equal system-type 'darwin)
  (set-frame-font "Cascadia Code 15")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin")))

(when (equal system-type 'gnu/linux)
  (cond
   ((getenv "WSLENV") (set-frame-font "Cascadia Code 11"))
   (t (set-frame-font "Cascadia Code 14"))))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (progn
	      (line-number-mode)
	      (column-number-mode)
              (display-line-numbers-mode)
	      (visual-line-mode))))
(put 'dired-find-alternate-file 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

