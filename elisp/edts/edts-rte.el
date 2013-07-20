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
  '(edts-mode)
  "*Modes affected by `edts-rte-for-modes'."
  :type '(repeat symbol) :group 'edts-rte)

(defun replace-rte-vars (&optional mode)
  (interactive)
  "Display the word `lambda' as the Greek letter.
Non-nil optional arg means use pretty-lambda display in that MODE.
nil means use pretty-lambda display for the current mode."
  (font-lock-add-keywords
   mode `(("\{\"__edts_rte__\",\\([^\(}\|,\)]+\\),\\([^\(}\|,\)]+\\)\}"
   (0 (progn (compose-region (match-beginning 0) (match-end 0)
                             (buffer-substring (match-beginning 2) (match-end 2)))
      nil))))))

;;(search-forward-regexp "\{\"__edts_rte__\",[^}]+\}")
;;(search-forward-regexp "\{\"__edts_rte__\",[^\(}\|,\)]+,[^\(}\|,\)]+\}")
;;(search-forward-regexp "\{\"__edts_rte__\",\\([^\(}\|,\)]+\\),\\([^\(}\|,\)]+\\)\}")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; edts-rte.el ends here

