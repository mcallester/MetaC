(setq *gdb* "/opt/local/bin/gdb-apple")
(setq *MetaC* "/Users/davidmcallester/MC/")

(require 'shell)

(define-prefix-command 'ctr-z-command 'ctr-z-map)

(add-hookm c-mode-hook
  (define-key c-mode-map "\C-z" 'ctr-z-command))

(define-key ctr-z-map "s" 'MC:start-metac)
(define-key ctr-z-map "x" 'MC:execute-cell)
(define-key ctr-z-map "r" 'MC:load-region)
(define-key ctr-z-map "B" 'MC:load-buffer-to-point)
(define-key ctr-z-map "b" 'MC:load-buffer)

(define-key ctr-z-map "a" 'MC:beginning-of-def)
(define-key ctr-z-map "p" 'MC:previous-def)
(define-key ctr-z-map "n" 'MC:next-def)
(define-key ctr-z-map "c" 'MC:goto-c)

(define-key ctr-z-map "c" 'MC:clean-cells)

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
    (error
     (beginning-of-buffer)
     (let ((c (char-after)))
       (when (or  (= c 32) (= c 47) (= c ?\t) (= c ?\n))
	 (MC:next-def))))))

(defun MC:previous-def ()
  (interactive)
  (MC:beginning-of-def)
  (backward-char)
  (MC:beginning-of-def))

(defun MC:next-def ()
  (interactive)
  (condition-case nil
      (progn (move-end-of-line nil)
	     (re-search-forward "\n[^] \n\t})/=]")
	     (move-beginning-of-line nil))
    (error (end-of-buffer))))

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

(defun gdb-buffer ()
  (get-buffer-create "*gdb*"))

(defun mc-process ()
    (get-buffer-process (gdb-buffer)))

(defun MC:null-filter (proc string) nil)

(defun MC:start-metac ()
  (interactive)
  (setq *source-buffer* (current-buffer))
  (when (mc-process) (delete-process (mc-process)))
  (with-current-buffer (gdb-buffer) (erase-buffer))
  (shell-command "rm /tmp/*")
  (setq *starting* t)
  (setq *compile-count* 1)
  (start-process "MetaC" (gdb-buffer) *gdb*)
  (with-current-buffer (gdb-buffer) (shell-mode))
  (set-process-filter (mc-process) (function MC:filter))
  (process-send-string (mc-process) (format "file %s/NIDE\n" *MetaC*))
  (process-send-string (mc-process) "break cbreak\n")
  (process-send-string (mc-process) "run\n")
  (setq *mc-accumulator* nil)
  (setq *gdb-mode* nil)
  (print "kernel restarted"))

(defun MC:execute-cell ()
  (interactive)
  (setq *source-buffer* (current-buffer))
  (setq *load-count* 1)
  (setq *beep-when-done* nil)
  (MC:execute-cell-internal))

(defun MC:load-region ()
  (interactive)
  (setq *source-buffer* (current-buffer))
  (let ((top (region-beginning)))
    (setq *load-count* (MC:num-cells-region))
    (setq *beep-when-done* t)
    (goto-char top)
    (if (zerop *load-count*)
        (message "Region contains no cell beginning")
      (MC:execute-cell-internal))))

(defun MC:execute-cell-internal ()
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

      (process-send-string (mc-process) (format "%s\0\n" exp))
      ;; the above return seems needed to flush the buffer
    )))

(defun MC:filter (proc string)
  (let ((clean  (MC:clean-string string)))
    ;;(print (list 'filter-receiving clean))
    (setq *mc-accumulator* (concat *mc-accumulator* clean))
    (MC:process-output)))

(defun MC:process-output ()
  (when (> (length *mc-accumulator*) 0)
    (let ((cell (MC:parse-output))) ;;when cell is not nil, this updates *mc-accumulator*
      (if cell
	(let ((tag (car cell))
	      (value (cdr cell)))
	  ;;(print '(**** doing) )  (print value)  (print tag)
	  (MC:dotag tag value)
	  ;;(print '(**** done))
	  (MC:process-output))
	(when *gdb-mode*
	  (insert *mc-accumulator*)
	  (set-marker (process-mark (mc-process)) (point))
	  (setq *mc-accumulator* nil))))))

(defun MC:insert-in-segment (value)
  (insert (replace-regexp-in-string "\n" "\n  " value)))

(defun MC:dotag (tag value)
  (cond ((string= tag "ignore"))

	((string= tag "result")
	 (setq *compile-count* (+ *compile-count* 1))
	 (if (> (length value) 1)
	     (MC:insert-in-segment (substring value 0 (- (length value) 1)))
	   (insert "value prints as empty string"))
	 (MC:next-def)
	 (setq *load-count* (- *load-count* 1))  ;;for load-region
	 (if (> *load-count* 0)
             (MC:execute-cell-internal)
           (when *beep-when-done* (beep))))

	;;the following two tags print
	((string= tag "print")
	 (print value))
	((string= tag "comp-error")
         (beep)
	 (MC:insert-in-segment "compilation error")
	 ;;(print value))
	 (with-current-buffer (message-buffer)
	   (erase-buffer) (MC:insert-in-segment value))
	 (display-buffer (message-buffer) 'display-buffer-pop-up-window))
	((string= tag "reader-error")
         (beep)
	 (MC:insert-in-segment "reader error")
	 ;;(print value))
	 (with-current-buffer (message-buffer)
	   (erase-buffer) (MC:insert-in-segment value))
	 (display-buffer (message-buffer) 'display-buffer-pop-up-window))

	;;the following three tags enter gdb mode
	((string= tag "exec-error")
         (beep)
	 (MC:insert-in-segment "dynamic-check execution error")
	 (MC:goto-gdb value))
	((string= tag "gdb-exec-error")
         (beep)
	 (MC:insert-in-segment "gdb-trapped execution error")
	 (MC:goto-gdb value))
	((string= tag "expansion-error")
         (beep)
	 (MC:insert-in-segment "macro expansion error")
	 (MC:goto-gdb value))
	((string= tag "breakpoint")
         (beep)
	 (MC:goto-gdb value))

	;;the tag IDE returns from from gdb mode
	((string= tag "IDE")
	 (pop-to-buffer *source-buffer*)
	 (when *gdb-mode* (delete-other-windows))
	 (setq *gdb-mode* nil)
	 (setq *starting* nil))

	(t (error (format "unrecognized tag %s" tag)))))

(defun MC:goto-gdb (value)
  (pop-to-buffer (gdb-buffer))
  (erase-buffer)
  (MC:insert-in-segment value)
  (set-marker (process-mark (mc-process)) (point))
  (setq *gdb-mode* t))

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
  (when (and (not *starting*)
	     (not *gdb-mode*)
	     (eq t (compare-strings "(gdb)" nil nil *mc-accumulator* -6 -1)))
    (let ((value *mc-accumulator*))
      (setq *mc-accumulator* nil)
      (cons "gdb-exec-error" value))))

(defun message-buffer ()
  (get-buffer-create "*MC Compiler Errors*"))

(defun MC:num-cells-region ()
  (save-excursion
    (let ((end (region-end))
          (beg (region-beginning)))
      (goto-char beg)
      (MC:beginning-of-def)
      (while (< (point) beg)
        (MC:next-def))
      (let ((count 0))
	(while (< (point) end)
	  (setq count (+ count 1))
	  (MC:next-def))
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
            (re-search-forward "**/" end)
            (forward-char)
            (kill-region (mark) (point))))
      (error nil))))

(defun MC:load-interval (start end)
  (set-mark start)
  (goto-char end)
  (MC:load-region))

(defun MC:load-buffer-to-point ()
  (interactive)
  (MC:load-interval (point-min) (point)))

(defun MC:load-buffer ()
  (interactive)
  (MC:load-interval (point-min) (point-max)))

(defun MC:goto-c ()
  (interactive)
  (find-file "/tmp")
  (revert-buffer)
  (end-of-buffer)
  (search-backward ".c")
  (backward-word)
  (dired-find-file)
  (end-of-buffer))
