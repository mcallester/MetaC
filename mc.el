(require 'shell)

(setq *gdb* "/opt/local/bin/gdb-apple")
(setq *MC-IDE* "~/18/MC/IDE")
(setq *seperator* "*#*#dsflsadk#*#*")

(setq auto-mode-alist
      (append
       (list (cons "\\.mc$"     'c-mode))
       auto-mode-alist))

(defun MC:beginning-of-def ()
  (interactive)
  (move-end-of-line nil)
  (condition-case nil
      (progn (re-search-backward "\n[^] \n\t})/=]")
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
	     (re-search-forward "\n[^] \n\t})/=]")
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
  (setq *source-buffer* (current-buffer))
  (when (mc-process) (delete-process (mc-process)))
  (with-current-buffer (mc-buffer) (erase-buffer))
  (start-process "MetaC" (mc-buffer) *gdb*)
  (with-current-buffer (mc-buffer) (shell-mode))
  (set-process-filter (mc-process) (function MC:filter))

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
	  (move-end-of-line nil)
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
      (process-send-string (mc-process) (format "%s\0\n" exp))))) ;; the return seems needed to flush the buffer

(defun MC:filter (proc string)
  (let ((clean  (MC:clean-string string)))
    (print (list 'filter-receiving clean))
    (setq *mc-accumulator* (concat *mc-accumulator* clean))
    (MC:process-output)))

(defun MC:process-output ()
  (when (> (length *mc-accumulator*) 0)
    (let ((cell (MC:parse-output))) ;;when cell is not nil, this updates *mc-accumulator*
      (if cell
	(let ((tag (car cell))
	      (value (cdr cell)))
	  (MC:dotag tag value)
	  (MC:process-output))
	(when *gdb-mode*
	  (insert *mc-accumulator*)
	  (set-marker (process-mark (mc-process)) (point))
	  (setq *mc-accumulator* nil))))))

(defun MC:insert-in-segment (value)
  (insert (replace-regexp-in-string "\n" "\n  " value)))

(defun MC:dotag (tag value)
  (print '****)
  (print value)
  (print tag)
  (print '****)
  (cond ((string= tag "ignore"))
	((string= tag "print")
	 (print value)
	((string= tag "result")
	 (MC:insert-in-segment (substring value 0 (- (length value) 1)))
	 (MC:next-def)
	 (setq *load-count* (- *load-count* 1))  ;;for load-region
	 (when  (> *load-count* 0)
	   (MC:load-definition-internal)))

	;;the following three tags enter gdb mode
	((string= tag "comp-error")
	 (MC:insert-in-segment "compilation error")
	 (with-current-buffer (message-buffer) (erase-buffer) (MC:insert-in-segment value))
	 (display-buffer (message-buffer) 'display-buffer-pop-up-window))
	((string= tag "exec-error")
	 (MC:insert-in-segment "execution error (running gdb)")
	 (setq *source-buffer* (current-buffer))
	 (pop-to-buffer (mc-buffer))
	 (erase-buffer)
	 (MC:insert-in-segment value)
	 (set-marker (process-mark (mc-process)) (point))
	 (setq *gdb-mode* t))
	((string= tag "breakpoint")
	 (setq *source-buffer* (current-buffer))
	 (pop-to-buffer (mc-buffer))
	 (erase-buffer)
	 (MC:insert-in-segment value)
	 (set-marker (process-mark (mc-process)) (point))
	 (setq *gdb-mode* t))

	;;the tag IDE returns from from gdb mode
	((string= tag "IDE")
	 (let ((w (get-buffer-window (mc-buffer))))
	   (when w (delete-window w)))
	 (pop-to-buffer *source-buffer*)
	 (setq *gdb-mode* nil))

	(t (error (format "unrecognized tag %s" tag)))))

(defun MC:clean-string (string)
  ;;removes carriage return chacters
  (let ((i 0))
    (dotimes (j (length string))
      (when (not (= (aref string j) 13)) ;;carriage return
	(aset string i (aref string j))
	(setq i (+ i 1))))
    (substring string 0 i)))

(defun MC:sep-pos (s i)
  (let ((s-length (length s))
	(sep-length (length *seperator*))
	(break nil)
	(val nil))
    (while (not break)
      (cond
       ((> (+ i sep-length) s-length)
	(setq break t)
	(setq val nil))
       ((eq t (compare-strings *seperator* 0 sep-length s i (+ i sep-length)))
	(setq break t)
	(setq val i))
       (t (setq i (+ i 1)))))
    val))

(defun MC:parse-output ()
  (let ((i (MC:sep-pos *mc-accumulator* 0)))
    (when i
      (let ((j (MC:sep-pos *mc-accumulator* (+ i (length *seperator*)))))
	(when j
	  (let ((value (substring *mc-accumulator* 0 i))
		(tag (substring *mc-accumulator* (+ i (length *seperator*)) j)))
	    (setq *mc-accumulator* (substring *mc-accumulator* (+ j (length *seperator*))))
	    (cons tag value)))))))

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
      (MC:insert-in-segment string)
      (comint-send-input))))

(defun MC:strip-cell-values ()
  (interactive)
  (save-excursion
    (condition-case nil
        (while t
          (beginning-of-buffer)
          (re-search-forward "/\\\*\\\* [0-9]*:")
          (beginning-of-line)
          (push-mark)
          (re-search-forward "\\\*\\\*/")
          (forward-char)
          (kill-region (mark) (point)))
      (error nil))))
