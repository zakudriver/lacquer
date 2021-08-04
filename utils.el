;;; utils.el --- Extend for lacquer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Some util function for lacquer.

;;; Install these required packages:

;; + cl-lib

;;; Code:


(require 'cl-lib)


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
    ""))


(defun lacquer-list-include (list target &optional which)
  "Whether TARGET is included in LIST, and return index or nil.
WHICH is a `nth' function to LIST."
  (cl-loop with i = 0
           for v in list
           if (eq (if (functionp which) (funcall which v) v)
                  target)
           return i
           else
           do (cl-incf i)
           finally return nil))


(defun lacquer-font-installed-p (name)
  "Check if string of NAME is available."
  (find-font (font-spec :name name)))


(defun lacquer-temporal-seconds (num temporal)
  "Mapping TEMPORAL of NUM to durations in seconds."
  (* num (cdr (assoc temporal timer-duration-words))))


(provide 'utils)

;;; utils.el ends here
