;; -*- common-lisp-style: modern -*-

(defpackage #:stumpwm.d/lib/volume
  (:documentation "Volume control via amixer(1).")
  (:use #:cl)
  (:export
   #:*mixer-command*
   #:*output-channel*
   #:*microphone-channel*
   #:*volume-step*
   #:*volume-change-output-filter-cmd*))
(in-package #:stumpwm.d/lib/volume)

(defparameter *mixer-command* "amixer")
(defparameter *output-channel* "Master")
(defparameter *microphone-channel* "Capture")
(defparameter *volume-step* "2%")
(defparameter *volume-change-output-filter-cmd* "egrep '\\[[[:digit:]]+%\\]'")

;;;-----------------------------------------------------------------------------

(defun format-volume-adjust-command (sign)
  "Return formatted shell command for adjusting volume up/down according to
sign."
  (format nil "~A set ~A ~A~A | ~A" *mixer-command* *output-channel*
          *volume-step* sign *volume-change-output-filter-cmd*))

(stumpwm:defcommand volume-up () ()
  (stumpwm:run-shell-command (format-volume-adjust-command "+") t))

(stumpwm:defcommand volume-down () ()
  (stumpwm:run-shell-command (format-volume-adjust-command "-") t))

;;;-----------------------------------------------------------------------------

(defun volume-toggle (channel)
  (format nil "~A set ~A toggle" *mixer-command* channel))

(stumpwm:defcommand volume-toggle-output () ()
  (stumpwm:run-shell-command (volume-toggle *output-channel*)))

(stumpwm:defcommand volume-toggle-microphone () ()
  (stumpwm:run-shell-command (volume-toggle *microphone-channel*)))
