(defun tf-change-surround-to
    (char)
  "Change the surrounding to the provided enclosure."
  (interactive "cChange to enclosure: ")
  (let (
        (pair (cond
               ((equal char ?') '("'" . "'"))
               ((equal char ?\") '("\"" . "\""))
               ((equal char ?\[) '("[" . "]"))
               ((equal char ?\]) '("[" . "]"))
               ((equal char ?\() '("(" . ")"))
               ((equal char ?\)) '("(" . ")"))
               ((equal char ?{) '("{" . "}"))
               ((equal char ?}) '("{" . "}"))
               ((equal char ?<) '("<" . ">"))
               ((equal char ?>) '("<" . ">"))
               ))
        loc)
    (save-excursion
      (backward-up-list 1 t t)
      (setq loc (point))
      (forward-sexp)
      (insert (cdr pair))
      (backward-char)
      (delete-char -1)
      (goto-char loc)
      (insert (car pair))
      (delete-char 1)
      )))

(defun tf-add-surround
    (char)
  "Add the surrounding to the active region."
  (interactive "cAdd enclosure: ")
  (let (
        (pair (cond
               ((equal char ?') '("'" . "'"))
               ((equal char ?\") '("\"" . "\""))
               ((equal char ?\[) '("[" . "]"))
               ((equal char ?\]) '("[" . "]"))
               ((equal char ?\() '("(" . ")"))
               ((equal char ?\)) '("(" . ")"))
               ((equal char ?{) '("{" . "}"))
               ((equal char ?}) '("{" . "}"))
               ((equal char ?<) '("<" . ">"))
               ((equal char ?>) '("<" . ">"))
               ))
        (start (min (point) (mark)))
        (end (max (point) (mark)))
    )
    (save-excursion
      (goto-char end)
      (insert (cdr pair))
      (goto-char start)
      (insert (car pair))
      )))


(defun tf-move-to-char (char)
  "Move to the provided char"
  (interactive "cMove to char: ")
  (forward-char)
  (cond
   ((equal char (char-after (point))) t)
   ((= (current-column) 0) t)
   (t (tf-move-to-char char))))

(defun tf-eval-ghci (exp)
  "Eval an expreesion in Haskell GHCI"
  (interactive (list (read-string "Haskell Exp: ")))
  (let final-exp (concat "stack ghci " exp ""))
  (message final-exp))

(defun tf-active-term-window ()
  "Return the active vterm window, return nil otherwise"
  (let ((term-window)
        (windows (window-list)))
    (dolist (window windows)
      (if (equal (buffer-name (window-buffer window)) eat-buffer-name)
          (setq term-window window)))
    term-window))

(defun tf-term ()
  "Invoke/dismiss terminal in a split window at the bottom"
  (interactive)
  (let ((term-window (tf-active-term-window)))
    (cond
     ((and term-window (= 1 (length (window-list)))) (message "Terminal is already fullscreen!"))
     (term-window (delete-window term-window))
     (t (split-window-below -15) (other-window 1) (eat)))))

(defun tf-term-cd ()
  "If a terminal window in visible, cd into the CWD of the other window in the terminal"
  (interactive)
  (let ((path)
        (term-window (tf-active-term-window)))
    (cond
     ((not term-window) (message "No active terminal"))
     ((= 1 (length (window-list))) (message "No other window"))
     (t (when (eq term-window (selected-window))
          (other-window 1))
        (setq path default-directory)
        (select-window term-window)
        (eat-line-send-input (concat "cd " path))
        ))))

(defun tf-move-line-down ()
  "Move a line down"
  (interactive)
  (let ((column (current-column)))
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (move-to-column column)
  ))

(defun tf-move-line-up ()
  "Move a line down"
  (interactive)
  (let ((column (current-column)))
  (transpose-lines 1)
  (forward-line -2)
  (move-to-column column)
  ))


(defun tf-keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(defun tf-toggle-highlight-symbol-at-point ()
  "Toggle highlighting for the symbol at point."
  (interactive)
  (when-let* ((regexp (find-tag-default-as-symbol-regexp)))
    (if (member regexp (hi-lock--regexps-at-point))
        ;; Unhighlight symbol at point
        (hi-lock-unface-buffer regexp)
      ;; Highlight symbol at point
      (hi-lock-face-symbol-at-point))))




