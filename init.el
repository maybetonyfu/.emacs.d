(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)


(setq default-frame-alist '((width . 150) (height . 50)))
(global-set-key (kbd "M-i") 'imenu)
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(fido-mode)
(global-tab-line-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(custom-enabled-themes '(doom-acario-light))
 '(custom-safe-themes t)
 '(haskell-compile-stack-build-command "stack build --fast")
 '(haskell-compiler-type 'stack)
 '(initial-buffer-choice t)
 '(make-backup-files nil)
 '(next-screen-context-lines 35)
 '(package-selected-packages '(dumb-jump haskell-mode doom-themes)))
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
