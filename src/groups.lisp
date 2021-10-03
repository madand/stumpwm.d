;; -*- common-lisp-style: modern -*-

(defpackage :stumpwm.d/groups
  (:documentation "Groups configuration.")
  (:use :cl)
  (:local-nicknames (#:swm :stumpwm))
  (:import-from :stumpwm
                #:kbd
                #:define-key
                #:*top-map*)
  (:import-from :stumpwm.d/lib/keyboard-dvorak))
(in-package :stumpwm.d/groups)

;;; ----------------------------------------------------------------------------
;;; Keybindings
;;; ----------------------------------------------------------------------------

;; Select groups with Win + digit (0-9).
(stumpwm.d/lib/keyboard-dvorak:do-digits-row (qwerty-key dvorak-key)
  (let ((command (format nil "gselect ~a" qwerty-key)))
    ;; Define the same keys for QWERTY, to be able to use them when we switch
    ;; keyboard layout for apps.
    (define-key *top-map* (kbd (format nil "s-~a" qwerty-key)) command)
    (define-key *top-map* (kbd (format nil "s-~a" dvorak-key)) command)))

;;;-----------------------------------------------------------------------------
;;; Additional groups
;;;-----------------------------------------------------------------------------

(defvar *desired-groups-count* 10
  "How many groups we want to end up with.")

(defun existing-groups-count (&optional (screen (swm:current-screen)))
  "Return count of existing groups of the screen."
  (length (swm:screen-groups screen)))

(defun ensure-groups-count (&optional (screen (swm:current-screen)))
  "Add new groups to the SCREEN until we have the desired number of them.
Each group is named after its number. See ‘*desired-groups-count*’."
  (loop :with existing-count := (existing-groups-count screen)
        :for n :from (1+ existing-count) :upto *desired-groups-count*
        :do (swm:add-group screen (write-to-string n) :background t)))

;; Create 9 additional groups, since StumpWM creates the first one by itself.
(ensure-groups-count)
