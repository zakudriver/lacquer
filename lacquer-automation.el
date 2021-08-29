;;; lacquer-automation.el --- Automation about lacquer  -*- lexical-binding: t; -*-

;; Copyright (C) 2017 dingansich_kum0

;; Author: dingansich_kum0 <zy.hua1122@outlook.com>
;; URL: https://example.com/package-name.el
;; Version: 1.0
;; Package-Requires: ((emacs "25.2") (ivy "0.13.4"))
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

;; Automation class of lacquer.

;;; Install these required packages:

;; + cl-lib
;; + eieio

;;; Code:


(require 'cl-lib)
(require 'eieio)
(require 'lacquer-utils)


(defclass lacquer-automation-cls ()
  ((lacquer-cls-time :initarg :time
                     :initform 0
                     :custom (choice
                              (const :tag "Orderly" 'orderly)
                              (const :tag "Random" 'random))
                     :documentation "Time.")
   (lacquer-cls-timer :initarg :timer
                      :initform nil
                      :custom timer
                      :documentation "Timer."))
  "Lacquer automation self.")


(cl-defmethod lacquer-cls-run ((this lacquer-automation-cls) func)
  "Run FUNC from THIS's time."
  (lacquer-cls-stop this)
  (let ((time (oref this lacquer-cls-time)))
    (cond
     ((integerp time)
      (oset this lacquer-cls-timer (run-at-time t time func)))
     ((listp time)
      (oset this lacquer-cls-time (sort time (lambda (a b) (< (lacquer-time-number a) (lacquer-time-number b)))))
      (lacquer-cls-time-list-run this func))
     (t
      (error "Slot 'time' of lacquer-automation-cls should be integer or list")))))


(cl-defmethod lacquer-cls-time-list-run ((this lacquer-automation-cls) func &optional next)
  "When THIS's time is list, run FUNC and NEXT."
  (let ((time (oref this lacquer-cls-time)))
    (unless (integerp next)
      (setq next (cl-loop with i = 0
                          for v in time
                          if (lacquer-compare-time v)
                          return i
                          else
                          do (cl-incf i)
                          finally return nil)))
    (if (and (integerp next) (= next (length time)))
        (setq next nil))

    (let ((tm (if (null next)
                  (lacquer-hhmm-to-time (car time) (lambda (list)
                                                     (let ((day-index (lacquer-time-list-index "day")))
                                                       (setf (nth day-index list) (+ (nth day-index list) 1))
                                                       list)))
                (nth next time))))
      (lacquer-cls-stop this)
      (oset this lacquer-cls-timer (run-at-time tm nil (lambda ()
                                                         (funcall func)
                                                         (lacquer-cls-time-list-run this func (+ next 1))))))))


(cl-defmethod lacquer-cls-stop ((this lacquer-automation-cls))
  "Stop THIS's timer task."
  (let ((timer (oref this lacquer-cls-timer)))
    (if (timerp timer)
        (cancel-timer timer)
      (oset this lacquer-cls-timer nil))))


(provide 'lacquer-automation)

;;; lacquer-automation.el ends here
