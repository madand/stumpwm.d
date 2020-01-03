;; -*- common-lisp-style: modern -*-

(defpackage #:stumpwm.d/autostart
  (:documentation "Autostart of various non StumpWM-related stuff.")
  (:use #:cl)
  (:local-nicknames
   (#:swm #:stumpwm)
   (#:vars #:stumpwm.d/variables)))
(in-package #:stumpwm.d/autostart)

(when swm:*initializing*
  ;; Set desktop background.
  (swm:run-shell-command (format nil "feh --bg-fill ~A" vars:*bg-image*))
  ;; Use pointer cursor, not the default X-shaped.
  (swm:run-shell-command "xsetroot -cursor_name left_ptr")
  ;; Load xmodmap(1) configuration.
  (swm:run-shell-command "xmodmap ~/.Xmodmap")
  ;; Picom is a fork of Compton: https://github.com/yshui/picom/
  (swm:run-shell-command "picom -b --dbus"))
