;;; utils.el --- Extend for lacquer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Some util function for lacquer.

;;; Install these required packages:

;; + cl-lib
;; + cl-seq

;;; Code:


(require 'cl-lib)
(require 'cl-seq)


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


(defun lacquer-temporal-seconds (num word)
  "Mapping WORD of NUM to durations in seconds."
  (* num (cdr (assoc word timer-duration-words))))


(defun lacquer-time-number (str)
  "STR of '12:00' to integer of 1200."
  (string-to-number (replace-regexp-in-string (regexp-quote ":") "" str)))


(defun lacquer-decoded-time (time word)
  "Like decoded-time-xxx(Emacs '27.1').
Get TIME object item by WORD."
  (let* ((words '("seconds" "minutes" "hour" "day" "month" "year" "dow" "dst" "zone"))
         (index (cl-position word words :test 'equal)))
    (nth index time)))


(provide 'utils)

;;; utils.el ends here
