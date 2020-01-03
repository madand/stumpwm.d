;; -*- common-lisp-style: modern -*-

(defpackage #:stumpwm.d/variables
  (:documentation "Global configuration variables.")
  (:use #:cl)
  (:import-from #:stumpwm)
  (:export
   #:*terminal-cmd*
   #:*font*
   #:*bg-image*))
(in-package #:stumpwm.d/variables)

;;;; StumpWM's variables

(setf stumpwm:*shell-program* "/bin/bash")
(setf stumpwm:*window-border-style* :thin)
(setf stumpwm:*mouse-focus-policy* :click)
(setf stumpwm:*ignore-wm-inc-hints* t)
(setf stumpwm:*frame-number-map* "hutenosa")
(setf stumpwm:*maxsize-border-width* 0)
(setf stumpwm:*normal-border-width* 0)

;;;; Custom variables

(defparameter *font* '(:family "Iosevka SS09" :subfamily "Regular" :size 14)
  "Font specification as initargs for 'clx-truetype:font.")

(defparameter *terminal-cmd* "urxvt"
  "Terminal emulator.")

(defvar *bg-image* "~/media/PhoneCamera/OpenCamera/IMG_20190825_180322.jpg"
  "Desktop background image.")
