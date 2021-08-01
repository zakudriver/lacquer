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


(defun lacquer-read-path (path)
  "Read PATH return string or nil."
  (if (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string))
    ""
    ))


(defun lacquer-is-existing (list target &optional which)
  "Check TARGET is exists in LIST.
WHICH is a `nth' function to LIST."
  (cl-loop for i in list
           when (eq
                 (if (functionp which) (funcall which i) i)
                 target)
           return t))


(defun lacquer-font-installed-p (name)
  "Check if string of NAME is available."
  (find-font (font-spec :name name)))


(provide 'utils)

;;; utils.el ends here
