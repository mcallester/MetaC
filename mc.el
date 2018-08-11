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

(defun MC:indent-def ()
  (interactive)
  (move-beginning-of-line nil)
  (let ((line (1+ (count-lines 1 (point)))))
    (MC:beginning-of-def)
    (let ((begining (point)))
      (MC:next-def)
      (let ((end (point)))
	(goto-char begining)
	(while (< (point) (- end 1))
	  (c-indent-line)
	  (next-line)
	  (move-beginning-of-line nil))
	(goto-line line)
	(c-indent-line)))))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-\M-q" 'MC:indent-def))
(defun mc-buffer ()
  (get-buffer-create "*MetaC*"))

(defun mc-process ()
    (get-buffer-process (mc-buffer)))

(defun MC:null-filter (proc string) nil)

(defun MC:start-metac ()
  (interactive)
  (when (mc-process) (delete-process (mc-process)))
  (with-current-buffer (mc-buffer) (erase-buffer))
  (start-process "MetaC" (mc-buffer) *gdb*)
    (with-current-buffer (mc-buffer) (shell-mode))
  (set-process-filter (mc-process) (function MC:null-filter))
  ;;(with-current-buffer (mc-buffer)
    ;;(MC-mode)
    ;;(when enable-multibyte-characters
    ;;(toggle-enable-multibyte-characters)))
  (process-send-string (mc-process) (format "file %s\n" *MC-IDE*))
  (process-send-string (mc-process) "break cbreak\n")
  (process-send-string (mc-process) "run\n")
  (setq *eval-count* 1)
  (setq *mc-accumulator* nil)
  (setq *gdb-mode* nil)
  (print "kernel restarted"))

(add-hookm c-mode-hook
  (define-key c-mode-map "\M-\C-s" 'MC:start-metac))

(defun MC:load-definition ()
  (interactive)
  (setq *load-count* 1)
  (MC:load-definition-internal))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-c\C-c" 'MC:load-definition))

(defun MC:load-region ()
  (interactive)
  (let ((top (region-beginning)))
    (setq *load-count* (MC:num-cells-region))
    (goto-char top)
    (MC:load-definition-internal)))

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-c\C-r" 'MC:load-region))

(defun MC:load-definition-internal ()
  (set-process-filter (mc-process) (function MC:filter))
  (delete-other-windows)
  (when *gdb-mode* (error "attempt to use IDE while in gdb breakpoint"))
  (setq buffer-file-coding-system 'utf-8-unix)
  (move-end-of-line nil)
  (MC:beginning-of-def)
  (let ((top (point)))
    (condition-case nil
	(progn (move-end-of-line nil)
	       (re-search-forward "\n[^] \n\t})]")
	       (backward-char))
      (error (end-of-buffer)))
    (re-search-backward "[^ \n\t]")
    (forward-char)
    (let ((exp (buffer-substring top (point))))
      (if (= (buffer-end 1) (point))
	  (insert "\n")
	(progn
	  (when (not (= (char-after (point)) 10)) ;;10 is return
	    (kill=line))
	  (if (= (buffer-end 1) (point))
	      (insert "\n")
	    (forward-char))))

      (if (string= (buffer-substring (point) (min (+ (point) 3) (point-max))) "/**")
	  (let ((start (point)))
	    (search-forward "*/")
	    (delete-region start (point)))
	(progn (newline) (backward-char)))

      (insert "/**  **/")
      (backward-char 4)

      (insert (format "%d: " *eval-count*))
      (setq *eval-count* (+ *eval-count* 1))
      
      (setq *source-buffer* (current-buffer))
      (setq *gdb-mode* nil)
      (process-send-string (mc-process) (format "%s\n" exp)))))

(defun MC:filter (proc string)
  (setq *mc-accumulator* (concat *mc-accumulator* (MC:clean-string string)))
  (MC:process-output))

(defun MC:process-output ()
  (when (> (length *mc-accumulator*) 0)
    (if (MC:contains-terminatorp *mc-accumulator*) ;;can return from gdb-mode
	(let* ((cell (MC:parse-output)) ;;this updates *mc-accumualtor*
	       (tag (car cell))
	       (value (cdr cell)))
	  (print "*****")
	  (print tag)
	  (print value)
	  (setq *gdb-mode* nil)
	  (MC:dotag tag value))
      (when *gdb-mode*
	(insert *mc-accumulator*)
	(set-marker (process-mark (mc-process)) (point))
	(setq *mc-accumulator* nil)))))

(defun MC:dotag (tag value)
  (cond ((string= tag "ignore")
	 (MC:process-output))
	((string= tag "result")
	 (insert value)
	 (MC:next-def)
	 (MC:process-output)
	 (setq *load-count* (- *load-count* 1))
	 (when  (> *load-count* 0)
	   (MC:load-definition-internal)))
	((string= tag "compilation error")
	 (insert "compilation error")
	 (with-current-buffer (message-buffer) (erase-buffer) (insert value))
	 (display-buffer (message-buffer) 'display-buffer-pop-up-window)
	 (MC:process-output))
	((string= tag "execution error")
	 (insert "execution error (running gdb)")
	 (setq *source-buffer* (current-buffer))
	 (pop-to-buffer (mc-buffer))
	 (erase-buffer)
	 (insert value)
	 (set-marker (process-mark (mc-process)) (point))
	 (setq *gdb-mode* t)
	 (MC:process-output))
	((string= tag "IDE") ;;returning from gdb session
	 (let ((w (get-buffer-window (mc-buffer))))
	   (when w (delete-window w)))
	 (pop-to-buffer *source-buffer*)
	 (setq *gdb-mode* nil)
	 (MC:process-output))
	(t (error (format "unrecognized tag %s" tag)))))

(defun MC:clean-string (string)
  ;;removes carriage return chacters
  (let ((i 0))
    (dotimes (j (length string))
      (when (not (= (aref string j) 13)) ;;carriage return
	(aset string i (aref string j))
	(setq i (+ i 1))))
    (substring string 0 i)))

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
      (let ((value (substring *mc-accumulator* 0 (max 0 (- i1 2))))
	    (tag (substring *mc-accumulator* i1 (- i 1))))
	(setq *mc-accumulator* (substring *mc-accumulator* i))
	(cons tag value)))))

(defun message-buffer ()
  (get-buffer-create "*MC-Messages*"))

(defun MC:num-cells-region ()
  (save-excursion
    (let ((end (region-end)))
      (goto-char (region-beginning))
      (let ((count 0))
	(while (< (point) end)
	  (setq count (+ count 1))
	  (MC:next-def))
	count))))

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

;;;the following three definitions are not currently used
;;;something like the first two will be needed when berror recursively
;;;invokes the REPL rather than gdb

(defun MC:return-key ()
  (interactive)
  (process-send-string (mc-process) "REPL\n")
  (comint-send-input))

(define-minor-mode MC-mode
  "the mode for Meta-C buffers built on shell-mode"
  :init-value nil
  :lighter "MC"
  :keymap  '(([?\r] . MC:return-key)))
  
(defun MC:command (string)
  (when (buffer-live-p (mc-buffer))
    (with-current-buffer (mc-buffer)
      (end-of-buffer)
      (insert string)
      (comint-send-input))))
