;; -*- common-lisp-style: modern -*-

(defpackage #:stumpwm.d/autostart
  (:documentation "Autostart of various non StumpWM-related stuff.")
  (:use #:cl)
  (:local-nicknames (#:swm #:stumpwm)))
(in-package #:stumpwm.d/autostart)

(when swm:*initializing*
  ;; Use pointer cursor, not the default X-shaped.
  (swm:run-shell-command "xsetroot -cursor_name left_ptr")

  ;; Picom is a fork of Compton: https://github.com/yshui/picom/
  (swm:run-shell-command "{ sleep 10 && picom -b --dbus ; } &")

  ;; Load xmodmap(1) configuration.
  (swm:run-shell-command "xmodmap ~/.Xmodmap"))
