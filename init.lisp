;; -*- common-lisp-style: modern -*-

(in-package #:stumpwm-user)

;;; ----------------------------------------------------------------------------
;;; Command for starting Slynk REPL server
;;; ----------------------------------------------------------------------------

;; We do this right here, rather than inside of the ‘stumpwm.d’ system, to
;; ensure the REPL is always available.

(defvar *repl-port* 4009
  "Port to start the REPL socket on.")

(stumpwm:defcommand start-repl () ()
  "Start REPL server on *REPL-PORT*."
  (ql:quickload "slynk")
  (handler-case
      (progn
        (funcall (intern (string '#:create-server) :slynk)
                 :port *repl-port*
                 :dont-close t)
        (stumpwm:message "REPL started on localhost:~d" *repl-port*))
    (sb-bsd-sockets:address-in-use-error ()
      (stumpwm:message "REPL already running on localhost:~d" *repl-port*))))

;;; ----------------------------------------------------------------------------
;;; Load ‘stumpwm.d’ system and cleanup environment variables
;;; ----------------------------------------------------------------------------

(defvar *qlot-unset-vars* '("CL_SOURCE_REGISTRY" "QUICKLISP_HOME" "ROS_OPTS"
                            "SBCL_HOME")
  "Environment variables to unset after initialization.

Since StumpWM process is a parent of all user session processes, we need to
unset these vars in order to prevent issues when starting other Lisp
processes.")

(unwind-protect
     (progn
       ;; Always start REPL.
       (start-repl)
       ;; Load a system with custom StumpWM configuration.
       (ql:quickload "stumpwm.d"))
  ;; Clean-up the environment after `ros qlot`.
  (dolist (env-var *qlot-unset-vars*)
    (sb-posix:unsetenv env-var)))
