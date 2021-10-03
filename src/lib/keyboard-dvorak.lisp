;; -*- common-lisp-style: modern -*-

(defpackage :stumpwm.d/lib/keyboard-dvorak
  (:documentation "Utilities for dealing with Programmer Dvorak layout.")
  (:use :cl)
  (:local-nicknames (#:a :alexandria))
  (:export
   #:do-digits-row
   #:+digits-to-programmer-dvorak+))
(in-package :stumpwm.d/lib/keyboard-dvorak)

(a:define-constant +digits-to-programmer-dvorak+
    '(("0" . "]")
      ("1" . "&")
      ("2" . "[")
      ("3" . "{")
      ("4" . "}")
      ("5" . "(")
      ("6" . "=")
      ("7" . "*")
      ("8" . ")")
      ("9" . "+"))
  :test 'equal
  :documentation
  "Mapping from QWERTY digit keys to the Programmer Dvorak counterparts.")

(defmacro do-digits-row ((qwerty-var dvorak-var &optional result-form)
                         &body body)
  "Iterate over mapping of QWERTY digit row to Programmer Dvorak counterparts.

On each iteration qwerty-var and dvorak-var will be bound to corresponding
single-character strings. The semantics of result-form and body are exactly
the same as the standard dolist macro."
  (a:with-gensyms (pair)
    `(dolist (,pair +digits-to-programmer-dvorak+ ,result-form)
       (let ((,qwerty-var (car ,pair))
             (,dvorak-var (cdr ,pair)))
         ,@body))))
