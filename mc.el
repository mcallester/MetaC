(setq auto-mode-alist
      (append
       (list (cons "\\.mc$"     'c-mode))
       auto-mode-alist))

(defun MC:beginning-of-def ()
  (interactive)
  (move-end-of-line nil)
  (condition-case nil
      (progn (re-search-backward "\n[^] \n\t})/]")
	     (forward-char))
    (error (beginning-of-buffer))))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-a" 'MC:beginning-of-def))

(defun MC:previous-def ()
  (interactive)
  (MC:beginning-of-def)
  (backward-char)
  (MC:beginning-of-def))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-b" 'MC:previous-def))

(defun MC:next-def ()
  (interactive)
  (condition-case nil
      (progn (move-end-of-line nil)
	     (re-search-forward "\n[^] \n\t})/]")
	     (move-beginning-of-line nil))
    (error (end-of-buffer))))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-n" 'MC:next-def))

(setq mc-process nil)

(setq process-adaptive-read-buffering nil)

(defun mc-buffer ()
  (get-buffer-create "*MetaC*"))

(defun MC:start-metac ()
  (interactive)
  (when mc-process (delete-process mc-process))
  (with-current-buffer (mc-buffer) (erase-buffer))
  (let ((process-connection-type t))
    (setq mc-process (start-process "MetaC" (mc-buffer) "/Users/davidmcallester/18/MC/IDE")))
  (with-current-buffer (mc-buffer) (shell-mode))
  (set-process-filter mc-process (function MC:filter))
  (setq *eval-count* 0))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-s" 'MC:start-metac))

(setq *code-buffer* nil)

(defun MC:load-definition ()
  (print 'got-to-load-def)
  (interactive)
  (move-end-of-line nil)
  (MC:beginning-of-def)
  (let ((top (point)))
    (re-search-forward "\n[^] \n\t})]")
    (backward-char)
    (re-search-backward "[^ \n\t]")
    (beginning-of-line 2)
    (let ((exp (buffer-substring top (point))))
      (if (string= (buffer-substring (point) (min (+ (point) 3) (point-max))) "/**")
	  (let ((start (point)))
	    (search-forward "*/")
	    (delete-region start (point)))
	(progn (newline) (backward-char)))
      (insert "/**  **/")
      (backward-char 4)
      (setq *eval-count* (+ *eval-count* 1))
      (insert (format "%d: " *eval-count*))
      (setq *code-buffer* (current-buffer))
      (process-send-string mc-process exp)
      (with-current-buffer (mc-buffer)
	(end-of-buffer)
	(insert exp)))))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-c\C-c" 'MC:load-definition))

(setq *continue* nil)

(defun MC:load-region ()
  (interactive)
  (setq *region-end* (region-end))
  (goto-char (region-beginning))
  (setq *continue* t)
  (MC:load-definition))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-c\C-r" 'MC:load-region))

(defun MC:filter (proc string)
  (print string)
  (if (or (< (length string) 15)
	     (not (string= (substring string -14 -4) "MC Success")))
      (progn
	(MC:insert-to-REPL proc string)
	(when *code-buffer*
	  (with-current-buffer *code-buffer*
	    (insert "Error: C-M-m will go to REPL in buffer *MetaC*"))))
    (progn
      (MC:insert-to-REPL proc (substring string 0 -14))
      (MC:insert-to-REPL proc "\nMC>")
      (when *code-buffer*
	(with-current-buffer *code-buffer*
	  (insert (substring string 0 -14))))))
  (MC:next-def)
  (if *continue*
      (if (< (point) *region-end*)
	  (MC:load-definition)
	(progn (setq *continue* nil)
	       (setq *code-buffer* nil)))
    (setq *code-buffer* nil)))

(defun MC:insert-to-REPL (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
	(save-excursion
	  ;; Insert the text, advancing the process marker.
	  (end-of-buffer)
	  (insert string)
	  (set-marker (process-mark proc) (point)))
	(if moving (goto-char (process-mark proc)))))))


(setq *source-buffer* nil)

(defun MC:goto-metac ()
  (interactive)
  (setq *source-buffer* (current-buffer))
  (switch-to-buffer (mc-buffer)))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-m" 'MC:goto-metac))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-c" 'beep))

(defun MC:goto-code ()
  (interactive)
  (switch-to-buffer *source-buffer*))

(defun mybeep ()
  (interactive)
  (beep))

(add-hookm shell-mode-hook
  (define-key shell-mode-map "\M-\C-c" 'MC:goto-code))

(add-hookm shell-mode-hook
  (define-key shell-mode-map "\M-\C-m" 'beep))

