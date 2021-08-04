;;; automation.el --- Automation about lacquer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Automation class of lacquer.

;;; Install these required packages:

;; + cl-lib
;; + eieio

;;; Code:


(require 'cl-lib)
(require 'eieio)
(require 'utils)


(defclass lacquer-automation-cls ()
  ((cls-time :initarg :cls-time
             :initform 0
             :type integer
             :custom integer
             :documentation "Time.")
   (cls-timer-list :initarg :cls-timer-list
                   :initform '()
                   :type list
                   :custom list
                   :documentation "List of timer."))
  "Lacquer automation self.")


(cl-defmethod cls-run ((this lacquer-automation-cls) func)
  "Run FUNC from THIS's time."
  (cls-stop this)
  (let ((time (oref this cls-time))
        (list (oref this cls-timer-list)))
    (when (integerp time)
      (let ((timer (run-at-time time time func)))
        (push list timer)))
    (when (listp time)
      (dolist (i time)
        (let ((timer (run-at-time i (lacquer-temporal-seconds 1 "day") func)))
          (push list timer))))))


(cl-defmethod cls-stop ((this lacquer-automation-cls))
  "Stop THIS's timer task."
  (let ((list (oref this cls-timer-list)))
    (when (listp list)
      (dolist (i list)
        (cancel-timer i)))
    (oset this cls-timer-list '())))


(provide 'automation)

;;; automation.el ends here
