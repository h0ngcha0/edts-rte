;;; edts-rte.el --- Replace a regex matched string in the buffer with
;;                  an customerized string of choice. Display another
;;                  customerized string of choice when the cursor is
;;                  located on top of the displayed string.
;;
;; Filename: edts-rte.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;;###autoload
(define-minor-mode edts-rte-mode
  "Display the replaced value returned by edts-rte.
When edts-rte replaces a variable with a value, a tuple in the format
of {\"__edts-rte__\", VarName, Value} is returned. Value should be displayed
and VarName should be displayed in the pop up window when the cursor is on
top of it"
  :init-value nil
  :lighter "-EDTS-RTE"
  (cond (edts-rte-mode
         (message "turning on")
         (highlight-rte-vars)
         (replace-rte-vars)
         (font-lock-fontify-buffer)
         )
        (t
         (message "turnning off")
         (font-lock-remove-keywords
          nil `((,(rte-regex)
                 (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                           (buffer-substring (match-beginning 2) (match-end 2)))
             nil)))))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward (rte-regex) nil t)
             (decompose-region (match-beginning 0) (match-end 0)))))))


(defun highlight-rte-vars (&optional mode)
  (interactive)
  "Highlight the tuple {\"__edts-rte__\", VarName, Value} returned by edts rte"
  (font-lock-add-keywords
   mode `((,(rte-regex) 0 'font-lock-warning-face prepend))))

(defun replace-rte-vars (&optional mode)
  (interactive)
  "Replace the tuple {\"__edts-rte__\", VarName, Value} returned by edts rte
with Value"
  (font-lock-add-keywords
   mode `((,(rte-regex)
           (0 (progn ;;(set-text-properties (match-beginning 0) (match-end 0) '(face hi-red-b))
                     (compose-region (match-beginning 0) (match-end 0)
                                     (buffer-substring (match-beginning 2) (match-end 2)))
                     nil))))))

(defun display-rte-var ()
  "Display the variable name in the tuple {\"__edts-rte__\", VarName, Value}
returned by edts rte"
  (interactive)
  (let* ((cur-point   (point))
         (displayed-p nil))
    (save-excursion
      (goto-line (point-min))
      (while (and (not displayed-p)
                  (re-search-forward (rte-regex) nil t))
        (if (and (>= cur-point (match-beginning 0))
                 (<= cur-point (match-end 0)))
            (progn (message (concat "Variable Name: "
                                    (buffer-substring (match-beginning 1) (match-end 1))))
                   (setq displayed-p t))
          (message ""))))))

(defadvice forward-char (after edts-rte-display-var)
  "Advice for forward-char for displaying the rte variable name"
  (display-rte-var))

;; Activate the forward-char advice
(ad-activate 'forward-char)

(defun rte-regex ()
  "Regex to match the return replaced vars from the edts-rte"
  "\{\"__edts_rte__\",\\([^\(}\|,\)]+\\),\\([^\(}\|,\)]+\\)\}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edts-rte.el ends here
