;; -*- common-lisp-style: modern -*-

(defpackage :stumpwm.d/utils
  (:documentation "StumpWM configuration utilities.")
  (:use :cl)
  (:local-nicknames
   (#:swm :stumpwm))
  (:import-from :stumpwm.d/variables
                #:*terminal-cmd*)
  (:export
   #:format-exec-in-terminal))
(in-package :stumpwm.d/utils)

(defun format-exec-in-terminal (command)
  "Build exec command with the terminal emulatior. See *TERMINAL-CMD*."
  (format nil "exec ~A ~A" *terminal-cmd* command))

;;; ----------------------------------------------------------------------------
;;; Simulation of mouse (rat) clicks
;;; ----------------------------------------------------------------------------

(defun send-rat-click (button iterations)
  "Send a mouse click to the current pointer location.
‘button’ is which mouse button to use and ‘iterations’ is how many times to
click (so twice would be a double-click, for example).

The implamentation requires xte utility program to be present in PATH. One need
to install xautomation(7) package, which provides xte."
  (loop :repeat iterations
        :do (shell-command (format nil "xte 'mouseclick ~A" button))))

(swm:defcommand rat-click (button iterations)
    ((:number "Button: ") (:number "How many times? "))
  (when (current-window)
    (send-rat-click button iterations)))
