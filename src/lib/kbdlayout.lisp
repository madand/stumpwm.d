;; -*- common-lisp-style: modern -*-

(defpackage #:stumpwm.d/lib/kbdlayout
  (:documentation "Per-window keyboard layout management via setxkbmap(1).")
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:swm #:stumpwm))
  (:import-from #:stumpwm)
  (:export
   #:configure-layouts
   #:register-hooks
   #:remove-hooks
   #:*run-xmodmap-p*))
(in-package #:stumpwm.d/lib/kbdlayout)

;;;-----------------------------------------------------------------------------
;;; Configuration variables
;;;-----------------------------------------------------------------------------

(defvar *run-xmodmap-p* t
  "Whether to run xmodmap(1) after every keyboard layout switch.")

(defvar *xmodmap-command* "xmodmap ~/.Xmodmap")

(defvar *setxkbmap-extra-options* ""
  "Additional options to be passed to setxkbmap(1).")

;;;-----------------------------------------------------------------------------
;;; keyboard-layout class definition, constructor, methods
;;;-----------------------------------------------------------------------------

(defclass keyboard-layout ()
  ((name
    :initarg :name
    :reader keyboard-layout-name
    :type simple-string)
   (xkb-layout
    :initarg :xkb-layout
    :reader keyboard-layout-xkb-layout
    :type simple-string)
   (xkb-variant
    :initarg :xkb-variant
    :reader keyboard-layout-xkb-variant
    :type simple-string))
  (:default-initargs
   :xkb-variant ""))

(defun make-keyboard-layout (name xkb-layout xkb-variant)
  "Constructor for keyboard-layout instances."
  (make-instance 'keyboard-layout :name name
                                  :xkb-layout xkb-layout
                                  :xkb-variant xkb-variant))

(defmethod print-object ((obj keyboard-layout) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a (~a ~a)"
            (keyboard-layout-name obj)
            (keyboard-layout-xkb-layout obj)
            (keyboard-layout-xkb-variant obj))))

;;;-----------------------------------------------------------------------------
;;; Layouts list with related utils
;;;-----------------------------------------------------------------------------

(defvar *layouts* (list (make-keyboard-layout "QWERTY" "us" ""))
  "List of keyboard layouts to cycle between.")

(defun layouts-default-layout ()
  "Return the default keyboard layout."
  (first *layouts*))

(defun configure-layouts (layouts-list)
  "Configure available keyboard layouts for windows to switch between.

LAYOUTS-LIST is a list of lists, each of form (name layout variant).
Where name is a layout name as will be show to the user, layout and variant will
be passed to setxkbmap(1) which see.

Example:
\(configure-layouts '((\"Programmer Dvorak\" \"us\" \"dvp\")
                     (\"QWERTY\" \"us\" \"\")))"
  (setf *layouts*
        (loop :for (name layout variant) :in layouts-list
              :collect (make-keyboard-layout name layout variant))))

(defun find-next-layout (layout)
  "Return the layout that \"follows\" the given one in *LAYOUTS* ring."
  (let* ((current-index (position layout *layouts*))
         (maybe-following-index (1+ current-index)))
    (if (< maybe-following-index (length *layouts*))
        (nth maybe-following-index *layouts*)
        (first *layouts*))))

;;;-----------------------------------------------------------------------------

;; Generate (window-kbdlayout win) and (setf (window-kbdlayout win) x)
(swm:define-window-slot "kbdlayout")

(defun get-window-layout (win)
  "Get current keyboard layout of the given window.

If the window hasn't a layout assigned yet, or it is invalid (due to
the configuration changes etc.), the default one will be set and returned."
  (multiple-value-bind (layout existed?) (window-kbdlayout win)
    ;; Check if the layout is currently present in *layouts*.
    (if (and existed? (member layout *layouts* :test 'eq))
        layout
        (setf (window-kbdlayout win) (layouts-default-layout)))))

;;;-----------------------------------------------------------------------------

(defun colorize-layout-name (name)
  "Add color control sequences for echo'ing the given name."
  (format nil "^74 ~a " name))

(defvar *setxkbmap-command-format* "setxkbmap ~a ~a ~a && ~a &")

(defvar *current-layout* nil
  "Track the current layout to prevent unnecessary switches.")

(defun switch-to-layout (layout)
  "Switch to the given keyboard layout.
Returns the passed layout argument."
  (check-type layout keyboard-layout)
  (swm:echo (colorize-layout-name (keyboard-layout-name layout)))
  (swm:run-shell-command (format nil *setxkbmap-command-format*
                                 (keyboard-layout-xkb-layout layout)
                                 (keyboard-layout-xkb-variant layout)
                                 *setxkbmap-extra-options*
                                 (if *run-xmodmap-p* *xmodmap-command* "true")))
  (setf *current-layout* layout))

(defun cycle-keyboard-layouts (win)
  "Cycle keyboard layouts for the given window (see ‘*layouts*’)."
  (let* ((curr-layout (get-window-layout win))
         (new-layout (find-next-layout curr-layout)))
    (switch-to-layout new-layout)
    (setf (window-kbdlayout win) new-layout)))

(swm:defcommand switch-keyboard-layout-current-window () ()
  "Cycle between keyboard layouts for the current window (see ‘*layouts*’)."
  (cycle-keyboard-layouts (swm:current-window)))

(defun focus-window-hook-handler (curr-win old-win)
  "Ensure proper keyboard layout for CURR-WIN.
This is a handler for `stumpwm:*focus-window-hook*', which see."
  (let* ((curr-layout (get-window-layout curr-win))
         (old-layout (if (typep old-win 'swm:window)
                         (get-window-layout old-win)
                         *current-layout*)))
    (unless (eq curr-layout old-layout)
      (switch-to-layout curr-layout))))

(defun register-hooks ()
  "Register hooks provided by this library."
  (if (consp *layouts*)
      (swm:add-hook swm:*focus-window-hook*
                    'focus-window-hook-handler)
      (swm::message-no-timeout
       "^71Keyboard layouts are not configured. See ~a:~a"
       (package-name (symbol-package 'configure-layouts))
       (symbol-name 'configure-layouts))))

(defun remove-hooks ()
  "Remove hooks registered by this library."
  (swm:remove-hook swm:*focus-window-hook*
                   'focus-window-hook-handler))
