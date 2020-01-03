;; -*- common-lisp-style: modern -*-

(defpackage :stumpwm.d/main
  (:documentation "Entry file for StumpWM configuration system.")
  (:nicknames :stumpwm.d)
  (:use :cl)
  (:local-nicknames
   (#:kbdlayout :stumpwm.d/lib/kbdlayout)
   (#:theme :stumpwm.d/theme))
  (:import-from :stumpwm
                #:kbd
                #:define-key
                #:*top-map*)
  (:import-from :stumpwm.d/variables)
  (:import-from :stumpwm.d/autostart)
  (:import-from :stumpwm.d/keybindings)
  (:import-from :stumpwm.d/groups))
(in-package :stumpwm.d/main)

;;; ----------------------------------------------------------------------------
;;; Per-window keyboard layout.
;;; ----------------------------------------------------------------------------

(kbdlayout:configure-layouts '(("Programmer Dvorak" "us" "dvp")
                               ("QWERTY" "us" "")
                               ("Українська" "ua,us" ",dvp")))

(kbdlayout:register-hooks)

;; XF86Launch1 is a ThinkVantage button on Thinkpad X220.
(define-key *top-map*
    (kbd "XF86Launch1") "switch-keyboard-layout-current-window")

;;; ----------------------------------------------------------------------------
;;; Theme and font.
;;; ----------------------------------------------------------------------------

(theme:setup-solarized-dark)
(theme:setup-font)
