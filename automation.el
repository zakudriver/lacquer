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
   (cls-timer :initarg :cls-timer
              :initform nil
              :documentation "Timer."))
  "Lacquer automation self.")


(cl-defmethod cls-run ((this lacquer-automation-cls) func)
  "Run FUNC from THIS's time."
  (cls-stop this)
  (let ((time (oref this cls-time)))
    (cond
     ((integerp time)
      (oset this cls-timer (run-at-time t time func)))
     ((listp time)
      (oset this cls-time (sort time (lambda (a b) (< (lacquer-time-number a) (lacquer-time-number b)))))
      (cls-time-list-run this func))
     (t
      (error "Slot 'cls-time' of lacquer-automation-cls should be integer or list")))))


(cl-defmethod cls-time-list-run ((this lacquer-automation-cls) func &optional next)
  "When THIS's time is list, run FUNC and NEXT."
  (let ((time (oref this cls-time)))
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
      (cls-stop this)
      (oset this cls-timer (run-at-time tm nil (lambda ()
                                                 (funcall func)
                                                 (cls-time-list-run this func (+ next 1))))))))


(cl-defmethod cls-stop ((this lacquer-automation-cls))
  "Stop THIS's timer task."
  (let ((timer (oref this cls-timer)))
    (if (timerp timer)
        (cancel-timer timer)
      (oset this cls-timer nil))))


(provide 'automation)

;;; automation.el ends here
