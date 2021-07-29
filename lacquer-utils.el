;;; utils.el --- Package description (Extend for lacquer)  -*- lexical-binding: t; -*-


;;; Code:


(defun lacquer-generate-keys-index-list (&optional prefix)
  "Generate keys index.
PREFIX is optional string."
  (let ((func 'number-to-string))
    (when (stringp prefix)
      (setq func
            (lambda (i)
              (concat prefix (number-to-string i)))))
    (mapcar func (number-sequence 1 9))))


(provide 'lacquer-utils)

;;; lacquer.el ends here
