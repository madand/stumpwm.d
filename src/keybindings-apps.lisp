;; -*- common-lisp-style: modern -*-

(defpackage #:stumpwm.d/keybindings-apps
  (:documentation "Keybindings for launching applications.")
  (:use #:cl)
  (:local-nicknames
   (#:swm #:stumpwm)
   (#:vars #:stumpwm.d/variables))
  (:import-from #:stumpwm
                #:kbd
                #:define-key
                #:*top-map*)
  (:import-from #:stumpwm.d/utils
                #:format-exec-in-terminal))
(in-package #:stumpwm.d/keybindings-apps)

;;; ----------------------------------------------------------------------------
;;; Top level bindings for most frequently used apps
;;; ----------------------------------------------------------------------------

(define-key *top-map* (kbd "s-RET") (format nil "exec ~A" vars:*terminal-cmd*))
(define-key *top-map* (kbd "s-b") "exec firefox")
(define-key *top-map* (kbd "s-c") (format-exec-in-terminal "twitch-curses"))
(define-key *top-map* (kbd "s-m") (format-exec-in-terminal "ncmpc"))
(define-key *top-map* (kbd "s-t") (format-exec-in-terminal "htop"))
(define-key *top-map* (kbd "s-v") (format-exec-in-terminal "vifm"))

;;; ----------------------------------------------------------------------------
;;; Apps keymap for less frequently used stuff
;;; ----------------------------------------------------------------------------

(defvar *apps-top-map* (swm:make-sparse-keymap))
(defvar *apps-browser-map* (swm:make-sparse-keymap))
(defvar *apps-device-map* (swm:make-sparse-keymap))
(defvar *apps-network-map* (swm:make-sparse-keymap))
(defvar *apps-system-map* (swm:make-sparse-keymap))

(define-key *top-map* (kbd "s-a") '*apps-top-map*)

(define-key *apps-top-map* (kbd "b") '*apps-browser-map*)
(define-key *apps-top-map* (kbd "d") '*apps-device-map*)
(define-key *apps-top-map* (kbd "n") '*apps-network-map*)
(define-key *apps-top-map* (kbd "s") '*apps-system-map*)

;;; ----------------------------------------------------------------------------
;;; Browsers
;;; ----------------------------------------------------------------------------

(define-key *apps-browser-map* (kbd "b") "exec firefox")
(define-key *apps-browser-map* (kbd "B") "exec firefox --private-window")
(define-key *apps-browser-map* (kbd "c") "exec chromium --incognito")
(define-key *apps-browser-map* (kbd "C") "exec chromium")
(define-key *apps-browser-map* (kbd "t") "exec tor-browser")

;;; ----------------------------------------------------------------------------
;;; Devices
;;; ----------------------------------------------------------------------------

(swm:defcommand removable-disks-info () ()
  (swm:run-shell-command
   "udiskie-info -a -o '{device_file}: {id_label} {mount_path}'" t))

(define-key *apps-device-map* (kbd "i") "removable-disks-info")
(define-key *apps-device-map* (kbd "m") "exec udiskie-mount -a")
(define-key *apps-device-map* (kbd "u") "exec udiskie-umount -a")

;;; ----------------------------------------------------------------------------
;;; Network
;;; ----------------------------------------------------------------------------

(define-key *apps-network-map* (kbd "n") (format-exec-in-terminal "nmtui"))

;;; ----------------------------------------------------------------------------
;;; System
;;; ----------------------------------------------------------------------------

(define-key *apps-system-map* (kbd "b") "exec sudo tlp-stat -b")
(define-key *apps-system-map* (kbd "m") (format-exec-in-terminal "ncpamixer"))
(define-key *apps-system-map* (kbd "n") (format-exec-in-terminal "nmtui"))
(define-key *apps-system-map* (kbd "p") "exec lxqt-config-powermanagement")
