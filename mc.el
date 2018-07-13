(setq *gdb* "/opt/local/bin/gdb-apple")
(setq *MC-IDE* "~/18/MC/IDE")

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

(defun mc-buffer ()
  (let ((buff (get-buffer-create "*MetaC*")))
    (with-current-buffer buff
      (setq major-mode 'shell-mode)
      (when enable-multibyte-characters
	(toggle-enable-multibyte-characters)))
    buff))

(defun mc-prpocess ()
    (get-buffer-process (mc-buffer)))

(defun MC:null-filter (proc string))

(defun MC:start-metac ()
  (interactive)
  (when (mc-process) (delete-process (mc-process)))
  (with-current-buffer (mc-buffer) (erase-buffer))
  (start-process-shell-command "MetaC" (mc-buffer) "gdb IDE")
  (MC:command "break cbreak")
  (MC:command "run")
  (set-process-filter (mc-process) (function MC:filter))
  (setq *eval-count* 1)
  (setq *prefix* nil))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-s" 'MC:start-metac))

(defun MC:command (string)
  (when (buffer-live-p (mc-buffer))
    (with-current-buffer (mc-buffer)
      (end-of-buffer)
      (insert string)
      (comint-send-input))))

(defun MC:insert-to-REPL (string)
  (when (buffer-live-p (mc-buffer))
    (with-current-buffer (mc-buffer)
      (end-of-buffer)
      (insert string)
      (set-marker (process-mark (mc-process)) (point)))))

(setq *mc-accumulator* nil)

(defun MC:filter (proc string)
  (if (eq (current-buffer) (mc-buffer))
      (comint-output-filter proc string)
    (progn
      (setq *mc-accumulator* (concat *mc-accumulator* string))
      (when (MC:contains-terminatorp *mc-accumulator*)
	(let ((cell (MC:parse-output)))
	  (funcall *continuation* (car cell) (cdr cell)))))))

(defun MC:contains-terminatorp (s)
  (let ((level 0))
    (dotimes (i (length s))
      (if (= (aref s i) ?})
	  (setq level (- level 1))
	(if (= (aref s i) ?{)
	    (setq level (+ level 1)))))
    (< level -1)))

(defun MC:parse-output ()
  (let ((level 0)
	(i 0))
    (while (not (= level -1))
      (if (= (aref *mc-accumulator* i) ?})
	  (setq level (- level 1))
	(if (= (aref *mc-accumulator* i) ?{)
	    (setq level (+ level 1))))
      (setq i (+ i 1)))
    (let ((i1 i))
      (while (not (= level -2))
	(if (= (aref *mc-accumulator* i) ?})
	    (setq level (- level 1))
	  (if (= (aref *mc-accumulator* i) ?{)
	      (setq level (+ level 1))))
	(setq i (+ i 1)))
      (let ((value (substring *mc-accumulator* 0 i1))
	    (tag (substring *mc-accumulator* (+ i1 1) i)))
	(setq *mc-accumulator* (substring *mc-accumulator* i2))
	(cons tag value)))))

(defun MC:load-definition-internal ()
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
      (insert (format "%d: " *eval-count*))
      (setq *eval-count* (+ *eval-count* 1))
      (MC:insert-to-REPL exp)
      (process-send-string (mc-process) exp))))

(defun MC:load-definition ()
  (interactive)
  (setq *continuation* 'MC:load-def-continuation)
  (MC:load-definition-internal))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-c\C-c" 'MC:load-definition))

(defun MC:error (text)
  (insert "error")
  (setq *code-buffer* (current-buffer))
  (pop-to-buffer (mc-buffer))
  (erase-buffer)
  (MC:insert-to-REPL text))

(defun MC:load-def-continuation (tag value)
  (unless (string= tag "ignore")
    (if (string= tag "error")
	(MC:error)
      (progn
	(insert value)
	(MC:next-def)))))

(defun MC:load-region ()
  (interactive)
  (let ((top (region-beginning)))
    (setq *load-count* (MC:num-cells-region))
    (goto-char top)
    (setq *continuation* 'load-reg-continuation)
    (MC:load-definition-internal)))

(defun load-reg-continuation (tag value)
  (unless (string= tag "ignore")
    (if (string= tag "error")
	(MC:error)
      (progn
	(MC:next-def)
	(setq *load-count* (- *load-count* 1))
	(when (> *load-count* 0)
	  (MC:load-definition-internal))))))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-c\C-r" 'MC:load-region))

(defun MC:num-cells-region ()
  (save-excursion
    (let ((end (region-end)))
      (goto-char (region-beginning))
      (let ((count 0))
	(while (< (point) end)
	  (setq count (+ count 1))
	  (MC:next-def))
	count))))

(setq *source-buffer* nil)

(defun MC:goto-metac ()
  (interactive)
  (setq *source-buffer* (current-buffer))
  (pop-to-buffer (mc-buffer)))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-m" 'MC:goto-metac))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-c" 'beep))

(defun MC:goto-code ()
  (interactive)
  (pop-to-buffer *source-buffer*))

(add-hookm shell-mode-hook
  (define-key shell-mode-map "\M-\C-c" 'MC:goto-code))

(add-hookm shell-mode-hook
  (define-key shell-mode-map "\M-\C-m" 'beep))

