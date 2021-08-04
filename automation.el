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
        (let ((timer (run-at-time (cls-resolve-time this i) (lacquer-temporal-seconds 1 "day") func)))
          (push list timer))))))


(cl-defmethod cls-stop ((this lacquer-automation-cls))
  "Stop THIS's timer task."
  (let ((list (oref this cls-timer-list)))
    (when (listp list)
      (dolist (i list)
        (when (timerp i)
          (cancel-timer i))))
    (oset this cls-timer-list '())))


(cl-defmethod cls-resolve-time ((this lacquer-automation-cls) time-str)
  "Resolve THIS's time.
If TIME-STR is past time in the tody, set to tomorrow."
  (let* ((hhmm (lacquer-time-number time-str))
         (now-code (decode-time))
         (now (current-time))
         (time (encode-time 0 (% hhmm 100) (/ hhmm 100)
                            (lacquer-decoded-time now-code "day")
				                    (lacquer-decoded-time now-code "month")
                            (lacquer-decoded-time now-code "year")
                            (lacquer-decoded-time now-code "zone"))))
    (if (time-less-p now time)
        time
      (time-add time (lacquer-temporal-seconds 1 "day")))))


(provide 'automation)

;;; automation.el ends here
