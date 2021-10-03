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
  (run-desktop-background-command)
  ;; Use pointer cursor, not the default X-shaped.
  (swm:run-shell-command "xsetroot -cursor_name left_ptr")
  ;; Load xmodmap(1) configuration.
  (swm:run-shell-command "xmodmap ~/.Xmodmap")
  ;; Disable DPMS and Screen Blanking.
  (swm:run-shell-command "xset s off -dpms")
  ;; Mouse Sensitivity.
  (swm:run-shell-command "xset m 2/1 1")
  ;; Satrt Emacs after 6 seconds.
  (swm:run-with-timer 6 nil #'swm:run-shell-command "emacs"))

;; Refresh the desktop background to prevent artifacts when hot-connecting a
;; monitor.
(swm:add-hook swm:*new-head-hook* 'run-desktop-background-command)
;; picom seems to stop working correctly after monitor hot-plug.
(swm:add-hook swm:*new-head-hook* 'restart-picom)
