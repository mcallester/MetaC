(defun goto-MCCrep ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer "*gud-REPL*")
  (end-of-buffer))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-co" 'goto-MCCrep))

(defun MCC:beginning-of-def ()
  (interactive)
  (move-beginning-of-line nil)
  (condition-case nil
      (progn (re-search-backward "\n[^]\[ \n\t{(})]")
	     (forward-char))
    (error (beginning-of-buffer))))

(defun MCC:next-def ()
  (interactive)
  (condition-case nil
      (progn (re-search-forward "\n[^]\[ \n\t{(})]")
	     (move-beginning-of-line nil))
    (error (end-of-buffer))))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-a" 'MCC:beginning-of-def))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-e" 'MCC:next-def))

(defun MCC:beginning-of-sec ()
  (interactive)
  (previous-line)
  (search-backward "/**")
  (previous-line)
  (recenter 0))

(defun MCC:end-of-sec ()
  (interactive)
  (next-line)
  (next-line)
  (search-forward "/**")
  (previous-line)
  (recenter nil))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-b" 'MCC:beginning-of-sec))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-n" 'MCC:end-of-sec))


(defun MCC:indent-def ()
  (interactive)
  (move-beginning-of-line nil)
  (let ((line (1+ (count-lines 1 (point)))))
    (MCC:beginning-of-def)
    (let ((begining (point)))
      (MCC:next-def)
      (let ((end (point)))
	(goto-char begining)
	(while (< (point) (- end 1))
	  (c-indent-line)
	  (next-line)
	  (move-beginning-of-line nil))
	(goto-line line)
	(c-indent-line)))))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-\M-q" 'MCC:indent-def))

(defun MCC:load-region ()
  (interactive)
  (let ((top (min (point) (mark)))
	(bottom (max (point) (mark))))

    (goto-MCCrep)
    (other-window 1)

    (goto-char top)
    (re-search-forward "[^ \n\t]")
    (backward-char)

    (other-window 1)
    (insert "load(\n")
    (other-window 1)
    
    (while (< (point) bottom)
      (set-mark-command nil)
      (MCC:next-def)
      (kill-ring-save (point) (mark) t)
      (other-window 1)
      (insert "{")
      (yank)
      (insert "},\n")
      (other-window 1))

    (other-window 1)
    (insert "nil)")
    (comint-send-input)
    (other-window 1)

    (goto-char top)
    (set-mark-command nil)
    (goto-char bottom)
    ))


(add-hookm c-mode-hook
  (define-key c-mode-map "\C-cr" 'MCC:load-region))

(defun MCC:load-definition ()
  (interactive)
  (let ((start (point)))
    (MCC:beginning-of-def)
    (let ((top (point)))
      (MCC:next-def)
      (kill-ring-save top (point))
      )
    (goto-MCCrep)
    (insert "load({\n")
    (yank)
    (insert "\n},nil)")
    (comint-send-input)
    (other-window 1)))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-c\C-c" 'MCC:load-definition))

(defun oil:show-arglist ()
  (interactive)
  (let ((here (point)))
    (search-backward "(")
    (forward-char)
    (kill-ring-save (point) here)
    (goto-lisp)
    (end-of-buffer)
    (insert "(emacs:defline '")
    (yank)
    (insert ")")
    (comint-send-input)
    (other-window 1)
    (goto-char here)))

(define-key lisp-mode-map "\C-c\C-a" 'oil:show-arglist)

(setq auto-mode-alist
      (append
       (list (cons "\\.mc$"     'c-mode))
       auto-mode-alist))





