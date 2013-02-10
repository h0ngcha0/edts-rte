(defun search-local-function-and-print (function arity)
  "Goto the definition of FUNCTION/ARITY in the current buffer."
  (let ((origin (point))
	(str (concat "\n" function "("))
	(searching t)
	start end)
    (goto-char (point-min))
    (while searching
      (cond ((search-forward str nil t)
	     (backward-char)
	     (when (or (null arity)
		       (eq (ferl-arity-at-point) arity))
	       (beginning-of-line)
	       (setq start  (point))
	       ;; output function body and goto original position
	       (erlang-end-of-function)
	       (setq end  (point))
	       (write-region start end "~/test.txt")
	       (goto-char origin)
	       (setq searching nil)))
	    (t
	     (setq searching nil)
	     (goto-char origin)
	     (if arity
		 (message "Couldn't find function %S/%S" function arity)
	       (message "Couldn't find function %S" function)))))))

(defun find-source (module function arity)
  "Find the source code for MODULE in a buffer, loading it if necessary.
When FUNCTION is specified, the point is moved to its start."
  (interactive)
  ;; Add us to the history list
  (let ((mark (copy-marker (point-marker))))
    (if (or (equal module (erlang-get-module))
            (string-equal module "MODULE"))
	;; try to look for function in current buffer
        (if function
            (progn
              (ring-insert-at-beginning (edts-window-history-ring) mark)
              (search-local-function-and-print function arity))
            (null (error "Function %s:%s/%s not found" module function arity)))
        ;; look for function in other files Issue
        (let* ((node (edts-project-buffer-node-name (current-buffer)))
               (info (edts-get-function-info node module function arity)))
          (if info
	      ;; if function is found, use temp buffer to open the source file and locate the function body and output.
	      (with-temp-buffer
		(progn
		  ;;(find-file-existing (cdr (assoc 'source info)))
		  (insert-file-contents (cdr (assoc 'source info)))
		  (ring-insert-at-beginning (edts-window-history-ring) mark)
		  (goto-line (cdr (assoc 'line info)))
		  ;;(write-region (point-min) (point-max) "~/test.txt")
		  (setq start (point))
		  (erlang-end-of-function)
		  (setq end (point))
		  (write-region start end "~/test.txt")
	      ))
              (null
               (error "Function %s:%s/%s not found" module function arity)
))))))

(defun find-source-under-point ()
  (interactive)
  (cond
   ;; look for a include/include_lib
   ((edts-header-under-point-p) (edts-find-header-source))
   ((edts-macro-under-point-p)  (edts-find-macro-source))
   ;; look for a M:F/A
   ((apply #'find-source
           (or (ferl-mfa-at-point) (error "No call at point."))))))

(add-hook 'before-save-hook 'find-source-under-point)
;; end region
