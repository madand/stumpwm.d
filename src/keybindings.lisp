;; -*- common-lisp-style: modern -*-

(defpackage #:stumpwm.d/keybindings
  (:documentation "Main key bindings.")
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:swm #:stumpwm))
  (:import-from #:stumpwm
                #:kbd
                #:define-key
                #:*top-map*)
  (:import-from #:stumpwm.d/lib/keyboard-dvorak)
  (:import-from #:stumpwm.d/lib/volume)
  (:import-from #:stumpwm.d/keybindings-apps))
(in-package #:stumpwm.d/keybindings)

(define-key *top-map* (kbd "s-TAB") "pull-hidden-other")
(define-key *top-map* (kbd "s-j") "next")
(define-key *top-map* (kbd "s-k") "prev")
(define-key *top-map* (kbd "s-o") "fnext")

;;; Window selection with keys F1-F10: F1 selects window 0 and so on.
(loop :for i :from 1 :to 10 :do
  (let ((key-chord (format nil "s-F~d" i))
        (command (format nil "select-window-by-number ~d" (1- i))))
    (define-key *top-map* (kbd key-chord) command)))

(define-key *top-map* (kbd "s-SPC") '*top-map*)

;;; Apps menu maps.
(defvar *apps-top-map* (swm:make-sparse-keymap))
(defvar *apps-browsers-map* (swm:make-sparse-keymap))
(defvar *apps-device-map* (swm:make-sparse-keymap))
(defvar *apps-system-map* (swm:make-sparse-keymap))
(defvar *apps-wifi-map* (swm:make-sparse-keymap))

(define-key *apps-top-map* (kbd "b") '*apps-browsers-map*)
(define-key *apps-top-map* (kbd "d") '*apps-device-map*)
(define-key *apps-top-map* (kbd "s") '*apps-system-map*)
(define-key *apps-top-map* (kbd "w") '*apps-wifi-map*)

(define-key *apps-browsers-map* (kbd "b") "exec firefox")
(define-key *apps-browsers-map* (kbd "c") "exec chromium")
(define-key *apps-browsers-map* (kbd "t") "exec tor-browser")

(swm:defcommand removable-disks-info () ()
  (swm:run-shell-command
   "udiskie-info -a -o '{device_file}: {id_label} {mount_path}'"
   t))
(define-key *apps-device-map* (kbd "i") "removable-disks-info")
(define-key *apps-device-map* (kbd "m") "exec udiskie-mount -a")
(define-key *apps-device-map* (kbd "u") "exec udiskie-umount -a")

(define-key *apps-system-map* (kbd "m") (format-exec-in-terminal "ncpamixer"))
(define-key *apps-system-map* (kbd "n") (format-exec-in-terminal "nmtui"))
(define-key *apps-system-map* (kbd "p") "exec lxqt-config-powermanagement")

(define-key *apps-wifi-map* (kbd "n") (format-exec-in-terminal "nmtui"))

;; XF86Launch1 == ThinkVantage button on Thinkpad X220
(define-key *top-map* (kbd "s-a") '*apps-top-map*)

(define-key *top-map* (kbd "s-f") "fullscreen")
(define-key *top-map* (kbd "s-n") "renumber")
(define-key *top-map* (kbd "s-m") (format-exec-in-terminal "ncmpc"))
(define-key *top-map* (kbd "s-q") "delete")
(define-key *top-map* (kbd "s-r") "exec")
(define-key *top-map* (kbd "s-;") "colon")
(define-key *top-map* (kbd "s-SPC") "windowlist")

;; Volume control.
(define-key *top-map* (kbd "XF86AudioLowerVolume") "volume-down")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "volume-up")
(define-key *top-map* (kbd "XF86AudioMute") "volume-toggle-output")
(define-key *top-map* (kbd "XF86AudioMicMute") "volume-toggle-microphone")

;; MPD control.
(define-key *top-map* (kbd "XF86AudioPlay") "exec mpc toggle")
(define-key *top-map* (kbd "XF86AudioNext") "exec mpc next")
(define-key *top-map* (kbd "XF86AudioPrev") "exec mpc prev")
(define-key *top-map* (kbd "XF86AudioStop") "exec mpc stop")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec brightness max")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec brightness min")

;; Menu mode bindings.
(define-key swm:*menu-map* (kbd "C-m") 'swm:menu-finish)
(define-key swm:*menu-map* (kbd "C-h") 'swm:menu-backspace)
(define-key swm:*menu-map* (kbd "C-j") 'swm:menu-down)
(define-key swm:*menu-map* (kbd "C-k") 'swm:menu-up)

(define-key swm:*menu-map* (kbd "TAB") 'swm:menu-down)
;; ISO_Left_Tab == Shift-TAB
(define-key swm:*menu-map* (kbd "ISO_Left_Tab") 'swm:menu-up)

;; Input mode bindings.
(define-key swm:*input-map* (kbd "C-m") 'swm::input-submit)
(define-key swm:*input-map* (kbd "C-h") 'swm::input-delete-backward-char)
(define-key swm:*input-map* (kbd "C-w") 'swm::input-backward-kill-word)
