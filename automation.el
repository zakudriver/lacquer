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
  ((lacquer-cls-time :initarg :time
                     :initform 0
                     :documentation "Time.")
   (lacquer-cls-timer :initarg :timer
                      :initform nil
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


(provide 'automation)

;;; automation.el ends here
