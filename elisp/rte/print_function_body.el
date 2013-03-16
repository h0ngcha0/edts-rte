(require 'rett-rest)

(defun rett-rest-put-editor (id body)
  (rett-rest-put "editors" id body))

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
	       ;; push function body on webserver, with parameters x y z method id code
	       (put-function-body-on-web-with-rest "1" "1" "1" "PUT" "10000" (buffer-substring start end))
	       ;;(write-region start end "~/test.txt")
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
		  ;; push function body on webserver, with parameters x y z method id code
		  (put-function-body-on-web-with-rest "1" "1" "1" "PUT" "10000" (buffer-substring start end))

		  ;;(write-region start end "~/test.txt")
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



(defun edts-rest-request-2 ()
  "Send a get request to RESOURCE with ARGS"
  (let (;;(url                       "http://localhost:8642/editors/1")
        (url                       "http://127.0.0.1:8642/editors/1")
        (url-request-method        "GET")
        ;;(url-request-extra-headers (list '("Content-Type" . "application/json")))
        ;;(url-request-extra-headers (list '("Content-Type" . "text/html")))
        ;;(url-request-data          "{\"x\":74,\"y\":92,\"z\":1,\"id\":\"1\",\"code\":\"i am a soldier\"}")
        (url-show-status           nil))
    ;;(edts-log-debug "Sending %s-request to %s" method url)
    (let ((buffer (url-retrieve-synchronously url)))
      (when buffer
        (with-current-buffer buffer
          (print_msg))))))
       ;;(with-current-buffer buffer
       ;;   (print_msg))))))

(edts-rest-request-2)

(defun print_msg()
  (print (buffer-substring (point-min) (point-max)))
)


;;(remove-hook 'before-save-hook 'find-source-under-point)
