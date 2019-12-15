;; -*- common-lisp-style: modern -*-

(defpackage :stumpwm.d/utils
  (:documentation "StumpWM configuration utilities.")
  (:use #:cl)
  (:import-from #:stumpwm.d/variables
                #:*terminal-cmd*)
  (:export
   #:format-exec-in-terminal))
(in-package #:stumpwm.d/utils)

(defun format-exec-in-terminal (command)
  "Build exec command with the terminal emulatior. See *TERMINAL-CMD*."
  (format nil "exec ~A -e '~A'" *terminal-cmd* command))
