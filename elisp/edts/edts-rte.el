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
(defcustom edts-rte-auto-modes
  '(erlang-mode)
  "*Modes affected by `edts-rte-for-modes'."
  :type '(repeat symbol) :group 'edts-rte)

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
         (replace-rte-vars)
         (font-lock-fontify-buffer)
         )
        (t
         (font-lock-remove-keywords
          nil `((,(rte-regex)
                 (0 (progn (compose-region (match-beginning 0) (match-end 0)
                                           (buffer-substring (match-beginning 2) (match-end 2)))
             nil)))))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward (rte-regex) nil t)
             (decompose-region (match-beginning 0) (match-end 0)))))))


(defun replace-rte-vars (&optional mode)
  (interactive)
  "Replace the tuple {\"__edts-rte__\", VarName, Value} returned by edts rte
with Value"
  (font-lock-add-keywords
   mode `((,(rte-regex)
           (0 (progn (set-text-properties (match-beginning 0) (match-end 0) '(face hi-red-b))
                     (compose-region (match-beginning 0) (match-end 0)
                                     (buffer-substring (match-beginning 2) (match-end 2)))
                     nil))))))

(defun rte-regex ()
  "Regex to match the return replaced vars from the edts-rte"
  "\{\"__edts_rte__\",\\([^\(}\|,\)]+\\),\\([^\(}\|,\)]+\\)\}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edts-rte.el ends here

