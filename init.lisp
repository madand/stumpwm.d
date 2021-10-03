;; -*- common-lisp-style: modern -*-

(in-package #:stumpwm-user)

;;; ----------------------------------------------------------------------------
;;; Command for starting Slynk REPL server
;;; ----------------------------------------------------------------------------

;; Initialize REPL right here, rather than inside of the ‘stumpwm.d’ system, to
;; ensure it is always available.

(defvar *repl-loaded-p* nil)

(defun load-repl ()
  (unless *repl-loaded-p*
    (load "~/.stumpwm.d/repl-helper.lisp")
    (setf *repl-loaded-p* t)))

(stumpwm:defcommand reload-slynk () ()
  "Reload Slynk server, presumably upgrading it."
  (load-repl)
  (handler-case
      (when (uiop:symbol-call '#:repl-helper '#:load-slynk :force-reload t)
        (stumpwm:message "Successfully reloaded Slynk."))
    (error (c)
      (dformat 0 "~a" c)
      (stumpwm:message "Error reloading Slynk. Check logs."))))

(stumpwm:defcommand start-repl () ()
  "Start REPL server on *REPL-PORT*."
  (load-repl)
  (handler-case
      (let ((port (uiop:symbol-call '#:repl-helper '#:start-slynk-repl)))
        (stumpwm:message "Started REPL on localhost:~a" port))
    (error (c)
      (dformat 0 "~a" c)
      (stumpwm:message "Error loading Slynk. Check the logs."))))

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
       ;; Always try to start the REPL.
       (handler-case
           (start-repl)
         (error (c)
           (dformat 0 "~a" c)
           (stumpwm:message "Failed to start REPL.~%~a" c)))
       ;; Load a system with custom StumpWM configuration.
       (ql:quickload "stumpwm.d"))
  ;; Clean-up the environment after `ros qlot`.
  (dolist (env-var *qlot-unset-vars*)
    (sb-posix:unsetenv env-var)))
