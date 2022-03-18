(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(smartparens-global-mode t)
(require 'smartparens-config)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq default-frame-alist '((width . 150) (height . 50)))

(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-o") 'window-swap-states)
(global-set-key (kbd "C-c p f") 'project-find-file)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "<f1>") 'shell)
(tool-bar-mode -1)
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
 '(custom-enabled-themes '(doom-acario-light))
 '(custom-safe-themes t)
 '(dired-dwim-target 'dired-dwim-target-next)
 '(haskell-compile-stack-build-command "stack build --fast")
 '(haskell-compiler-type 'stack)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(make-backup-files nil)
 '(next-screen-context-lines 35)
 '(package-selected-packages
   '(orderless whole-line-or-region selectrum-prescient selectrum smartparens dumb-jump haskell-mode doom-themes))
 '(sp-base-key-bindings 'sp)
 '(standard-indent 2)
 '(tab-always-indent t)
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Cascadia Code"))))
 '(whitespace-line ((t nil))))

(add-hook 'prog-mode-hook
	  (lambda ()
	    (progn
	      (line-number-mode)
	      (column-number-mode)
              (display-line-numbers-mode)
	      (visual-line-mode))))
(put 'dired-find-alternate-file 'disabled nil)
