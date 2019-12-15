;; -*- common-lisp-style: modern -*-

(in-package #:stumpwm-user)

;;; Add command for starting a slynk REPL server.
;; We do this right here, not inside of the config system, to ensure the REPL
;; is available even when the config system fails to build/load.

(defvar *repl-port* 4009
  "Port to start the REPL socket on.")

(stumpwm:defcommand start-repl () ()
  "Start REPL server on *REPL-PORT*."
  (ql:quickload "slynk")
  (handler-case
      (progn
        (funcall (intern (string '#:create-server) '#:slynk)
                 :port *repl-port*
                 :dont-close t)
        (stumpwm:message "REPL started on localhost:~d" *repl-port*))
    (sb-bsd-sockets:address-in-use-error ()
      (stumpwm:echo "REPL is aready running."))))



;; Load a system with custom StumpWM configuration.
(ql:quickload "stumpwm.d")

;; Clean-up the environment after `ros qlot`.
(dolist (env-var '("CL_SOURCE_REGISTRY" "QUICKLISP_HOME" "ROS_OPTS"))
  (sb-posix:unsetenv env-var))
