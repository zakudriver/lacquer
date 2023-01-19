;;; lacquer-utils.el --- Extend for lacquer  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 zakudriver

;; Author: zakudriver <zy.hua1122@gmail.com>
;; URL: https://github.com/zakudriver/lacquer
;; Version: 1.0
;; Package-Requires: ((emacs "25.2"))
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Some util function for lacquer.

;;; Install these required packages:

;; + cl-lib
;; + cl-seq

;;; Code:


(require 'cl-lib)


(defun lacquer-generate-a-z ()
  "Generate string list a-z."
  (mapcar #'char-to-string (number-sequence ?a ?z)))


(defun lacquer-generate-keys-index-list (&optional prefix)
  "Generate keys index.
PREFIX is optional string."
  (let ((func (lambda (i)
                (if (numberp i) (number-to-string i) i)))
        (suffixs (append (number-sequence 1 9) (lacquer-generate-a-z))))
    (if (stringp prefix)
        (mapcar (lambda (i)
                  (concat prefix (funcall func i))) suffixs)
      (mapcar func suffixs))))


(defun lacquer-read-path (path)
  "Read PATH return string or nil."
  (if (file-exists-p path)
      (with-temp-buffer (insert-file-contents path) (buffer-string))
    ""))


(defun lacquer-font-installed-p (name)
  "Check if string of NAME is available."
  (if (symbolp name)
      (setq name (symbol-name name)))
  (find-font (font-spec :name name)))


(defun lacquer-time-word-seconds (num word)
  "Mapping WORD of NUM to durations in seconds."
  (* num (cdr (assoc word timer-duration-words))))


(defun lacquer-time-number (str)
  "STR of '12:00' to integer of 1200."
  (if (stringp str)
      (string-to-number (replace-regexp-in-string (regexp-quote ":") "" str))
    0))


(defun lacquer-time-list-index (word)
  "Get index of time list by WORD."
  (let ((words '("seconds" "minutes" "hour" "day" "month" "year" "dow" "dst" "zone")))
    (cl-position word words :test 'equal)))


(defun lacquer-decoded-time (time word)
  "Like decoded-time-xxx(Emacs '27.1').
Get TIME object item by WORD."
  (nth (lacquer-time-list-index word) time))


(defun lacquer-hhmm-to-time (hhmm &optional func)
  "Convert HHMM to time.
Callback FUNC is handle to time list."
  (if (stringp hhmm)
      (setq hhmm (lacquer-time-number hhmm)))
  (let* ((now (decode-time))
         (time-code (list 0 (% hhmm 100) (/ hhmm 100)
                          (lacquer-decoded-time now "day")
				                  (lacquer-decoded-time now "month")
                          (lacquer-decoded-time now "year")
                          (lacquer-decoded-time now "zone"))))
    (if (functionp func)
        (setq time-code (funcall func time-code)))
    (apply #'encode-time time-code)))


(defun lacquer-time-add (hhmm seconds)
  "HHMM + SECONDS.
TIME is `hh:mm' or hhmm.
Return timer."
  (time-add (lacquer-hhmm-to-time hhmm)
            seconds))


(defun lacquer-compare-time (hhmm)
  "Compare now and HHMM.
If now less than time return t."
  (let ((now (current-time))
        (time (lacquer-hhmm-to-time hhmm)))
    (time-less-p now time)))


(defun lacquer-mapc-incf (func list)
  "MAP to incf index.
FUNC and LIST like `mapc'."
  (let ((index -1))
    (mapc (lambda (arg)
            (cl-incf index)
            (funcall func arg index)) list)))


(provide 'lacquer-utils)

;;; lacquer-utils.el ends here
