;; -*- common-lisp-style: modern -*-

(defpackage :stumpwm.d/autostart
  (:documentation "Autostart of various non StumpWM-related stuff.")
  (:use :cl)
  (:local-nicknames
   (#:swm :stumpwm)
   (#:vars :stumpwm.d/variables)))
(in-package :stumpwm.d/autostart)

(defun desktop-background-command ()
  "Return shell command that sets the desktop background."
  (format nil "feh --bg-max ~A" vars:*bg-image*))

(defun run-desktop-background-command (&rest _)
  "Set the desktop background via a shell command.
See the function `desktop-background-command'. The ignored arg is for the
compatibility with `stumpwm::*new-head-hook*'."
  (declare (ignore _))
  (swm:run-shell-command (desktop-background-command)))

(defun restart-picom (&rest _)
  "Restart the `picom' compositor."
  (declare (ignore _))
  (swm:run-shell-command "pkill $(pidof picom)"))

(when swm:*initializing*
  ;; Set desktop background.
  (swm:run-shell-command (format nil "feh --bg-fill ~A" vars:*bg-image*))
  ;; Use pointer cursor, not the default X-shaped.
  (swm:run-shell-command "xsetroot -cursor_name left_ptr")
  ;; Load xmodmap(1) configuration.
  (swm:run-shell-command "xmodmap ~/.Xmodmap")

;; picom seems to stop working correctly after monitor hot-plug.
(swm:add-hook swm:*new-head-hook* 'restart-picom)
