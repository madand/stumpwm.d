;; -*-lisp-*-

(in-package :stumpwm-user)

;;;; Slynk REPL server.

(defvar *slynk-port* 4009
  "Port to start the REPL socket on.")

(ql:quickload "slynk")
(slynk:create-server :port *slynk-port*
                     :dont-close t)

;;;;
(add-to-load-path "~/.stumpwm.d/modules/")

;; (define-key )
