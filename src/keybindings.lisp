;; -*- common-lisp-style: modern -*-

(defpackage :stumpwm.d/keybindings
  (:documentation "Main key bindings.")
  (:use :cl)
  (:local-nicknames
   (#:a :alexandria)
   (#:swm :stumpwm))
  (:import-from :stumpwm
                #:*root-map*
                #:kbd
                #:define-key
                #:*top-map*)
  (:import-from :stumpwm.d/lib/keyboard-dvorak)
  (:import-from :stumpwm.d/lib/volume)
  (:import-from :stumpwm.d/keybindings-apps))
(in-package :stumpwm.d/keybindings)

(swm:set-prefix-key (kbd "s-SPC"))

;;; ----------------------------------------------------------------------------
;;; Global shortcuts
;;; ----------------------------------------------------------------------------

(define-key *top-map* (kbd "s-TAB") "pull-hidden-other")
(define-key *top-map* (kbd "s-j") "next")
(define-key *top-map* (kbd "s-k") "prev")
(define-key *top-map* (kbd "s-o") "fnext")

(define-key *root-map* (kbd "w") "windowlist")

;; Window selection with keys F1-F12: F1 selects window 0 and so on.
(loop :for i :from 1 :upto 12
      :as key := (format nil "s-F~d" i)
      :as command := (format nil "select-window-by-number ~d" (1- i))
      :do (define-key *top-map* (kbd key) command))

(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-n") "renumber")
(define-key *top-map* (kbd "s-q") "delete")
(define-key *top-map* (kbd "s-r") "exec")
(define-key *top-map* (kbd "s-;") "colon")

;;; ----------------------------------------------------------------------------
;;; Volume control
;;; ----------------------------------------------------------------------------

(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-output")
(define-key *top-map* (kbd "XF86AudioMicMute") "volume-toggle-microphone")

;;; ----------------------------------------------------------------------------
;;; MPD control
;;; ----------------------------------------------------------------------------

(define-key *top-map* (kbd "XF86AudioPlay") "exec mpc toggle")
(define-key *top-map* (kbd "XF86AudioNext") "exec mpc next")
(define-key *top-map* (kbd "XF86AudioPrev") "exec mpc prev")
(define-key *top-map* (kbd "XF86AudioStop") "exec mpc stop")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec brightness max")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec brightness min")

;;; ----------------------------------------------------------------------------
;;; Menu mode bindings
;;; ----------------------------------------------------------------------------

(define-key swm:*menu-map* (kbd "C-m") 'swm:menu-finish)
(define-key swm:*menu-map* (kbd "C-h") 'swm:menu-backspace)
(define-key swm:*menu-map* (kbd "C-j") 'swm:menu-down)
(define-key swm:*menu-map* (kbd "C-k") 'swm:menu-up)

(define-key swm:*menu-map* (kbd "TAB") 'swm:menu-down)
;; ISO_Left_Tab == Shift-TAB
(define-key swm:*menu-map* (kbd "ISO_Left_Tab") 'swm:menu-up)

;;; ----------------------------------------------------------------------------
;;; Input mode bindings
;;; ----------------------------------------------------------------------------

(define-key swm:*input-map* (kbd "C-m") 'swm::input-submit)
(define-key swm:*input-map* (kbd "C-h") 'swm::input-delete-backward-char)
(define-key swm:*input-map* (kbd "C-w") 'swm::input-backward-kill-word)
