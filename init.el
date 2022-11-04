(setq custom-file (concat user-emacs-directory "custom.el"))
;; Packages format-all dumb-jump eglot doom-themes doom-modeline haskell-mode all-the-icons flycheck magit company

(load custom-file t)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;(unless package-archive-contents
;  (package-refresh-contents))

(when (display-graphic-p)
  (tool-bar-mode 0)
  (scroll-bar-mode 0))
(setq default-frame-alist '((width . 120) (height . 30)))
(setq inhibit-startup-screen t)
(column-number-mode)
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)

(fido-mode)
(fido-vertical-mode) ;; Make suggestion list vertical, as in selectrum
(global-tab-line-mode)
(global-visual-line-mode)
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq show-paren-delay 0)
(show-paren-mode)

(global-company-mode)
(setq auto-save-file-name-transforms '((".*" "~/.tmp/emacs/auto-save/" t)))
(setq backup-directory-alist '(("." . "~/.tmp/emacs/backup/")))
(setq backup-by-copying t)
(setq create-lockfiles nil)

(add-hook 'prog-mode-hook
          (lambda ()
            (progn
              (display-line-numbers-mode)
              (make-local-variable 'show-trailing-whitespace)
              (setq show-trailing-whitespace t))))

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (eglot)
;;             ))

(defun tf-toggle-previous-letter-case ()
  "Toggle the letter case of the letter to the left of cursor."
  (interactive)
  (let ((case-fold-search nil))
    (left-char 1)
    (cond
     ((looking-at "[[:lower:]]") (upcase-region (point) (1+ (point))))
     ((looking-at "[[:upper:]]") (downcase-region (point) (1+ (point)))))
    (right-char)))

(doom-modeline-mode 1)

(defun scroll-up-small ()
  (interactive)
  (scroll-up 15))

(defun scroll-down-small ()
  (interactive)
  (scroll-down 15))

(global-set-key (kbd "C-v") 'scroll-up-small)
(global-set-key (kbd "M-v") 'scroll-down-small)

(global-set-key (kbd "C-c C-t") 'tf-toggle-previous-letter-case)
(global-set-key (kbd "C-c C-i") 'set-selective-display)

(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
