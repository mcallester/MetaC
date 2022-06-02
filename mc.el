(setq *gdb* "/usr/bin/gdb")
(setq *MetaC* "/home/david/MC/")

(require 'shell)


(define-derived-mode mc-mode
  c-mode "mc-mode"
  "Major mode for meta-c"
  (define-key mc-mode-map "\C-xc" 'make-section)
  (define-key mc-mode-map "\M-\C-u" 'MC:beginning-of-sec)
  (define-key mc-mode-map "\M-\C-n" 'MC:end-of-sec)
  
  (define-key mc-mode-map "\C-\M-s" 'MC:start-metac)
  (define-key mc-mode-map "\C-\M-x" 'MC:execute-cell)
  (define-key mc-mode-map "\C-\M-r" 'MC:load-region)
  (define-key mc-mode-map "\C-\M-a" 'MC:beginning-of-cell)
  (define-key mc-mode-map "\C-\M-g" 'MC:indent-cell) ;;also used for end-of-cell
  (define-key mc-mode-map "\C-\M-b" 'MC:next-cell)
  (define-key mc-mode-map "\C-\M-c" 'MC:clean-cells) ;;will use region
  (define-key c-mode-map "\C-x`" 'MC:display-error)
  (define-key mc-mode-map "\C-x'"   'MC:last-compilation)
  (define-key shell-mode-map "\C-x`" 'MC:display-gdb-source)
  (define-key mc-mode-map [?\r] 'MC:return)
  (define-key mc-mode-map [?\t] 'MC:tab)

  (define-key mc-mode-map [?}] 'self-insert-command)
  (define-key mc-mode-map [?{] 'self-insert-command)
  (define-key mc-mode-map [?\(] 'self-insert-command)
  (define-key mc-mode-map [?\)] 'self-insert-command)
  (define-key mc-mode-map [?\[] 'self-insert-command)
  (define-key mc-mode-map [?\]] 'self-insert-command)
  (define-key mc-mode-map [?:] 'self-insert-command)
  (define-key mc-mode-map [?,] 'self-insert-command)
  (define-key mc-mode-map [?\;] 'self-insert-command))


(setq auto-mode-alist
      (append
       (list (cons "\\.mc$" 'mc-mode))
       (list (cons "\\.mz$" 'mc-mode))
       auto-mode-alist))


(defun make-section ()
  (interactive)
  (end-of-line)
  (newline)
  (insert "/** ========================================================================")
  (newline)
  (insert "========================================================================**/")
  (previous-line 1)
  (end-of-line)
  (newline))

(defun MC:beginning-of-sec () (interactive) (previous-line)
  (search-backward "/** =") (previous-line))

(defun MC:end-of-sec () (interactive) (next-line) (next-line)
  (search-forward "/** =") (previous-line))

(defun MC:in-commentp ()
  (save-excursion
    (condition-case nil
	(let ((here (point)))
	  (search-backward "/**")
	  (search-forward "**/")
	  (if (> (point) here) t nil))
      (error nil))))

(defun MC:next-cell ()
  (interactive)
  (condition-case nil
      (progn (move-end-of-line nil)
	     (re-search-forward "\n[^])} \n\t\/]")
	     (while (MC:in-commentp)
	       (re-search-forward "\n[^])} \n\t\/]"))
	     (move-beginning-of-line nil))
    (error (end-of-buffer))))

(defun MC:beginning-of-cell ()
  (interactive)
  (condition-case nil
      (progn (re-search-backward "\n[^])} \n\t\/]")
	     (while (MC:in-commentp)
	       (re-search-backward "\n[^])} \n\t\/]"))
	     (forward-char))
    (error (beginning-of-buffer))))

(defun MC:return ()
  (interactive)
  (insert-char ?\n)
  (unless (MC:in-commentp)
    (MC:tab)))

(defun MC:tab ()
  (interactive)
  (catch 'tab
      (setq *init-line* (line-number-at-pos))
      (MC:beginning-of-cell)
      (MC:pprint-paren-tab 0 0)))


(defun MC:pprint-paren-tab (indent close-char)
  ;;(print (list 'pprint-tab indent close-char))
  (let ((c (char-after)))
    (while c
      (forward-char)
      (cond ((>= (line-number-at-pos) *init-line*)
	     (delete-horizontal-space)
	     (indent-to indent)
	     (when (and (char-after)
			(closep (char-after))
			(not (= (char-after) close-char)))
	       (beep))
	     (throw 'tab nil))
	    ((= c 10) ;;newline
	     (delete-horizontal-space)
	     (indent-to indent)
	     (setq c (char-after)))
	    ((closep c)
	     (when (or (= indent 0)
		       (not (= c close-char)))
	       (backward-char)
	       (beep)
	       (throw 'tab nil))
	     (setq c nil))
	    ((openp c)
	     (if (= c 40) ;;open paren
		 (progn (MC:pprint-paren-tab (current-column) 41) ;;close paren
			(setq c (char-after)))
	       (progn (MC:pprint-paren-tab (+ indent 2) (close-for c))
		      (setq c (char-after)))))
	    (t (setq c (char-after)))))))

(defun MC:indent-cell ()
  (interactive)
  (catch 'cell
    (beginning-of-line)
    (when (member (char-after) '(32 9 10))
      (MC:beginning-of-cell))
    (MC:pprint-paren-cell 0 0)))

(defun MC:pprint-paren-cell (indent close-char)
  (let ((c (char-after)))
    (while c
      (forward-char)
      (cond ((= c 10) ;;newline
	     (cond ((= indent 0) (setq c nil))
		   (t (when (and (char-after)
				 (not (member (char-after) (list 32 9 10))))
			(beep)
			(throw 'cell nil))
		      (delete-horizontal-space)
		      (indent-to indent)
		      (setq c (char-after)))))
	    ((closep c)
	     (when (not (= c close-char))
	       (backward-char)
	       (beep)
	       (throw 'cell nil))
	     (setq c nil))
	    ((openp c)
	     (if (= c 40) ;;open paren
		 (progn (MC:pprint-paren-cell (current-column) 41) ;;close paren
			(setq c (char-after)))
	       (progn (MC:pprint-paren-cell (+ indent 2) (close-for c))
		      (setq c (char-after)))))
	    (t (setq c (char-after)))))))



(defun openp (c)
  (or (= c 40) ;;paren
      (= c 123) ;;curly bracket
      (= c 91))) ;;brace

(defun closep (c)
  (or (= c 41) ;;paren
      (= c 125) ;;curly bracket
      (= c 93))) ;;brace

(defun spacep (c)
  (or (= c 32) ;space
      (= c 9) ;tab
      ))

(defun close-for(paren)
  (cond ((= paren 40) 41) ;;paren
	((= paren 123) 125) ;;curly bracket
	((= paren 91) 93))) ;;brace

(defun gdb-buffer ()
  (get-buffer-create "*gdb*"))

(defun mc-process ()
    (get-buffer-process (gdb-buffer)))

;;The gdb process initialization printout sometmes arrives after MC:start-metac exits.
;;We must wait for the start-up process to finish to avoid the start-up
;;printout from interfearing with inter-process communication.

(defun MC:start-metac ()
  (interactive)
  (MC:clean-cells)
  (setq *waiting* t) ;;this is needed to avoid parsing "(gdb)" as a segment fault during startup
  (setq *gdb-mode* nil)
  (setq *mc-accumulator* nil)
  (setq *load-count* 0)
  (when (mc-process) (delete-process (mc-process)))
  (with-current-buffer (gdb-buffer) (erase-buffer))
  (shell-command "rm /tmp/TEMP*")
  (start-process "MetaC" (gdb-buffer) "/usr/bin/bash")
  (with-current-buffer (gdb-buffer) (shell-mode))
  (set-process-filter (mc-process) (function MC:filter))
  (process-send-string (mc-process) "gdb\n")
  (process-send-string (mc-process) (format "file %s/NIDE\n" *MetaC*))
  (process-send-string (mc-process) "break cbreak\n")
  (process-send-string (mc-process) "run\n"))

(defun MC:execute-cell ()
  (interactive)
  (setq *source-buffer* (current-buffer))
  (setq *load-count* 1)
  (MC:execute-cell-internal))

(defun MC:load-region ()
  (interactive)
  (setq *source-buffer* (current-buffer))
  (let ((top (region-beginning)))
    (setq *load-count* (MC:num-cells-region))
    (if (zerop *load-count*)
	(message "Region contains no first cell")
      (progn
	(goto-char top)
	(MC:next-cell)
	(MC:execute-cell-internal)))))

(defun MC:execute-cell-internal ()
  (cond (*waiting*
	 (print "Meta-C not ready (emacs is waiting for kernel to return)")
	 (beep))
	(*gdb-mode*
	 (print "Meta-C not ready (emacs is waiting for gdb to return))")
	 (beep))
	(t
	 (delete-other-windows)
	 (setq buffer-file-coding-system 'utf-8-unix)
	 (move-end-of-line nil)
	 (MC:beginning-of-cell)
	 
	 (let ((top (point)))
	   (condition-case nil
	       (progn (move-end-of-line nil)
		      (re-search-forward "\n[^] \n\t})]")
		      (backward-char))
	     (error (end-of-buffer)))
	   (re-search-backward "[^ \n\t]")
	   (forward-char)
	   (when (= top (point)) (error "there is no cell"))
	   
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
	     (setq *source-buffer* (current-buffer))
	     (setq *value-point* (point))
					;(print 'sending exp)
	     (setq *waiting* t)
	     (setq *load-count* (- *load-count* 1))
	     (process-send-string (mc-process) (format "%s\0\n" exp))
	     ;; the above return seems needed to flush the buffer
	     )))))
	
(defun MC:filter (proc string)
  (let ((clean  (MC:clean-string string)))
    ;(print (list '*waiting* *waiting* 'filter-receiving clean))
    (setq *mc-accumulator* (concat *mc-accumulator* clean))
    (MC:process-output)))
	
(defun MC:process-output ()
  (when (> (length *mc-accumulator*) 0)
    (let ((cell (MC:parse-output))) ;;when cell is not nil, this updates *mc-accumulator*
      (if cell
	(let ((tag (car cell))
	      (value (cdr cell)))
	  ;(print (list '**** 'doing tag))
	  ;(print value)
	  (MC:dotag tag value)
	  ;(print '(**** done))
	  (MC:process-output))
	(when *gdb-mode*
	  (insert *mc-accumulator*)
	  (set-marker (process-mark (mc-process)) (point))
	  (setq *mc-accumulator* nil))))))

(defun MC:dotag (tag value)
  (cond ((string= tag "mc-ready")
	 (setq *mc-accumulator* nil)
	 (setq *waiting* nil)
	 (cond ((= *load-count* 0)
		(print '(kernel ready)))
	       (t
		(MC:execute-cell-internal))))
	(t (MC:dotag_other tag value)
	   (process-send-string (mc-process) "tag_done\0\n"))))

(defun MC:dotag_other (tag value)
  (cond	((string= tag "reader-error")
         (beep)
	 (MC:insert-value "reader error")
	 (MC:display-abort-message value))

	((string= tag "expansion-error")
         (beep)
	 (MC:insert-value "mc to c dynamic-check error")
	 (MC:goto-gdb value))

	((string= tag "comp-error")
         (beep)
	 (MC:insert-value "c compilation error")
	 (MC:display-abort-message value))

	((string= tag "exec-error")
         (beep)
	 (MC:insert-value "dynamic-check error")
	 (MC:goto-gdb value))
	
	((string= tag "gdb-exec-error")
         (beep)
	 (MC:insert-value "segment fault --- to resume type p NIDE()")
	 (MC:goto-gdb value))

	((string= tag "breakpoint")
         (beep)
	 (MC:goto-gdb value))

	((string= tag "continue-from-gdb")
	 (MC:continue-from-gdb))

	((string= tag "result")
	 (MC:insert-value (substring value 0 (- (length value) 1)))
	 (MC:next-cell))

	((string= tag "uncaught-throw")
	 (beep)
	 (MC:insert-value "uncaught throw"))

	((string= tag "ignore"))

	((string= tag "print")
	 (print value))
	
	(t (error (format "unrecognized tag %s" tag)))))

(defun mc-fix (msg)
  (replace-regexp-in-string "\n" "\n  " value))

(defun MC:insert-value (value)
  (with-current-buffer *source-buffer*
    (goto-char *value-point*)
    (insert (mc-fix value))))

(defun MC:display-abort-message (msg)
  (if (get-buffer "*MC compilation*")
      (kill-buffer "*MC compilation*"))
  (let ((buffer (get-buffer-create "*MC compilation*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "  ")
      (insert (mc-fix msg))
      (beginning-of-buffer)
      (compilation-mode))
    (display-buffer buffer 'display-buffer-pop-up-window)))

(defun MC:display-error ()
  (interactive)
  (let ((starting-buffer (current-buffer)))
    (catch 'done
      (other-window 1)
      (switch-to-buffer "*MC compilation*")
      (condition-case nil
	  (progn (re-search-forward (rx (or "error:" "warning:")))
		 (setq found t))
	(error
	 (progn (print "moved past last error")
		(other-window 1)
		(switch-to-buffer starting-buffer)
		(throw 'done nil))))
      (beginning-of-line)
      (re-search-forward "/")
      (let ((p1 (point)))
	(search-forward ":")
	(let ((p2 (point)))
	  (let ((file (buffer-substring (- p1 1) (- p2 1))))
	    (search-forward ":")
	    (let ((p3 (point)))
	      (let ((line (string-to-number (buffer-substring p2 (- p3 1)))))
		(search-forward ":")
		(let ((p4 (point)))
		  (setq c (string-to-number (buffer-substring p3 (- p4 1))))
		  (forward-line)
		  (other-window 1)
		  (find-file file)
		  (goto-line line)
		  (move-to-column (- c 1)))))))))))

(defun MC:display-gdb-source ()
  (interactive)
  (re-search-backward "/")
  (re-search-backward "/")
  (let ((p1 (point)))
    (search-forward ":")
    (let ((p2 (point)))
      (let ((file (buffer-substring p1 (- p2 1))))
	(move-end-of-line nil)
	(let ((p3 (point)))
	  (let ((line (string-to-number (buffer-substring p2 p3))))
	    (other-window 1)
	    (find-file file)
	    (goto-line line)))))))

(defun tmp-buffer ()
  (get-buffer-create ""))

(defun last-compiled-file ()
  (let ((b (current-buffer)))
    (find-file "/tmp")
    (revert-buffer t t)
    (beginning-of-buffer)
    (let ((largest 0))
      (catch 'done
	(while t
	  (condition-case nil
	      (search-forward " TEMP")
	    (error (throw 'done nil)))
	  (let ((p1 (point)))
	    (search-forward ".")
	    (let ((s  (buffer-substring p1 (- (point) 1))))
	      (setq largest (max largest (string-to-number s)))))))
      (pop-to-buffer b)
      (format "/tmp/TEMP%d.c" largest))))
	   
(defun MC:last-compilation ()
  (interactive)
  (find-file (last-compiled-file))
  (end-of-buffer)
  (delete-other-windows))

(defun MC:goto-gdb (value)
  (pop-to-buffer (gdb-buffer))
  (erase-buffer)
  (insert (mc-fix value))
  (set-marker (process-mark (mc-process)) (point))
  (setq *gdb-mode* t)
)

(defun MC:fixup-gdb ()
  (newline)
  (insert "(gdb)")
  (set-marker (process-mark (mc-process)) (point)))


(defun MC:continue-from-gdb ()
  (delete-windows-on (gdb-buffer))
  (pop-to-buffer *source-buffer*)
  (setq *gdb-mode* nil))


(defun MC:clean-string (string)
  ;;removes carriage return chacters
  (let ((i 0))
    (dotimes (j (length string))
      (when (not (= (aref string j) 13)) ;;carriage return
	(aset string i (aref string j))
	(setq i (+ i 1))))
    (substring string 0 i)))

(setq *seperator* "*#*#dsflsadk#*#*")

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

(defun MC:parse-output()
  (or (MC:parse-output1) (MC:parse-output2)))
  
(defun MC:parse-output1 ()
  (let ((i (MC:sep-pos *mc-accumulator* 0)))
    (when i
      (let ((j (MC:sep-pos *mc-accumulator* (+ i (length *seperator*)))))
	(when j
	  (let ((value (substring *mc-accumulator* 0 i))
		(tag (substring *mc-accumulator* (+ i (length *seperator*)) j)))
	    (setq *mc-accumulator* (substring *mc-accumulator* (+ j (length *seperator*))))
	    (cons tag value)))))))

(defun MC:parse-output2 ()
  (when (and (not *waiting*)
	     (not *gdb-mode*)
	     (eq t (compare-strings "(gdb)" nil nil *mc-accumulator* -6 -1)))
    (let ((value *mc-accumulator*))
      (setq *mc-accumulator* nil)
      (cons "gdb-exec-error" value))))

(defun message-buffer ()
  (get-buffer-create "*MC compilation*"))

(defun MC:num-cells-region ()
  (save-excursion
    (let ((end (region-end))
          (beg (region-beginning)))
      (goto-char beg)
      (MC:next-cell)
      (let ((count 0))
	(while (< (point) end)
	  (setq count (+ count 1))
	  (MC:next-cell))
	count))))

(defun MC:clean-cells ()
  (interactive)
  (save-excursion
    (condition-case nil
        (let ((start (if (use-region-p) (region-beginning) (point-min)))
              (end (if (use-region-p) (region-end) (point-max))))
          (while t
            (goto-char start) 
            (re-search-forward "/** [0-9]+:" end)
            (beginning-of-line)
            (push-mark)
            (search-forward "**/" end)
            (forward-char)
            (kill-region (mark) (point))))
      (error nil))))




		    
	   
	  
	 
	   
	      

		;;
							       
(defun MZ:alphap (c)
    (or (and (>= c ?A) (<= c ?Z))
	(and (>= c ?a) (<= c ?z))
	(and (>= c ?0) (<= c ?9))
	(= c ?_)))

(defun MZ:connp (c)
  (or (= c ?\; )
      (= c ?,)
      (= c ?:)
      (= c ?@)
      (= c ?|)
      (= c ?&)
      (= c ?!)
      (= c ??)
      (= c ?=)
      (= c ?~)
      (= c ?<)
      (= c ?>)
      (= c ?+)
      (= c ?-)
      (= c ?*)
      (= c ?/)
      (= c ?%)
      (= c ?^)
      (= c ?#)
      (= c ?.)))

(defun MZ:skip_white ()
  (let ((c (char-after)))
    (while (and (<=c 32) ;; non-printing
		(not (= c 12))) ;form feed
      (forward-char)
      (setq c (char-after)))))
