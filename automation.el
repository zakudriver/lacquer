;;; automation.el --- Automation about lacquer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Automation class of lacquer.

;;; Install these required packages:

;; + cl-lib
;; + eieio
;; + timer

;;; Code:


(require 'cl-lib)
(require 'eieio)


(defconst lacquer-day-seconds (cdr (assoc "day" timer-duration-words))
  "Mapping day to durations in seconds.")


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
      (oset this cls-timer-list (append (list (run-at-time time time func)) list)))
    (when (listp time)
      (dolist (i time)
        (oset this cls-timer-list (append (list (run-at-time time lacquer-day-seconds func)) list))))))


(cl-defmethod cls-stop ((this lacquer-automation-cls))
  "Stop THIS's timer task."
  (let ((list (oref this cls-timer-list)))
    (when (listp list)
      (dolist (i list)
        (cancel-timer i)))
    (oset this cls-timer-list '())))


(provide 'automation)

;;; automation.el ends here
