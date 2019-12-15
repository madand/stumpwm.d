;; -*- common-lisp-style: modern -*-

(defpackage #:stumpwm.d/groups
  (:documentation "Groups configuration.")
  (:use #:cl)
  (:local-nicknames (#:swm #:stumpwm))
  (:import-from #:stumpwm
                #:kbd
                #:define-key
                #:*top-map*)
  (:import-from #:stumpwm.d/lib/keyboard-dvorak))
(in-package #:stumpwm.d/groups)

;;; ----------------------------------------------------------------------------
;;; Keybindings.
;;; ----------------------------------------------------------------------------

;; Select groups with Win + digit (0-9).
(stumpwm.d/lib/keyboard-dvorak:do-digits-row (qwerty-key dvorak-key)
  (let ((command (format nil "gselect ~a" qwerty-key)))
    ;; Define the same keys for QWERTY, to be able to use them when we switch
    ;; keyboard layout for apps.
    (define-key *top-map* (kbd (format nil "s-~a" qwerty-key)) command)
    (define-key *top-map* (kbd (format nil "s-~a" dvorak-key)) command)))

;;;-----------------------------------------------------------------------------
;;; Group definitions.
;;;-----------------------------------------------------------------------------
