;; -*- common-lisp-style: modern -*-

(defpackage :stumpwm.d/theme
  (:documentation "Solarized Dark colors and font setup.")
  (:use :cl)
  (:local-nicknames
   (#:a :alexandria)
   (#:swm :stumpwm)
   (#:vars :stumpwm.d/variables))
  (:import-from :clx-truetype)
  (:import-from :ttf-fonts)
  (:export
   #:+base03+
   #:+base02+
   #:+base01+
   #:+base00+
   #:+base0+
   #:+base1+
   #:+base2+
   #:+base3+
   #:+yellow+
   #:+orange+
   #:+red+
   #:+magenta+
   #:+violet+
   #:+blue+
   #:+cyan+
   #:+green+
   #:setup-solarized-dark
   #:setup-font))
(in-package :stumpwm.d/theme)

;; https://ethanschoonover.com/solarized/#usage-development
;; SOLARIZED HEX     16/8 TERMCOL  XTERM/HEX   L*A*B      RGB         HSB
;; --------- ------- ---- -------  ----------- ---------- ----------- -----------
;; base03    #002b36  8/4 brblack  234 #1c1c1c 15 -12 -12   0  43  54 193 100  21
;; base02    #073642  0/4 black    235 #262626 20 -12 -12   7  54  66 192  90  26
;; base01    #586e75 10/7 brgreen  240 #585858 45 -07 -07  88 110 117 194  25  46
;; base00    #657b83 11/7 bryellow 241 #626262 50 -07 -07 101 123 131 195  23  51
;; base0     #839496 12/6 brblue   244 #808080 60 -06 -03 131 148 150 186  13  59
;; base1     #93a1a1 14/4 brcyan   245 #8a8a8a 65 -05 -02 147 161 161 180   9  63
;; base2     #eee8d5  7/7 white    254 #e4e4e4 92 -00  10 238 232 213  44  11  93
;; base3     #fdf6e3 15/7 brwhite  230 #ffffd7 97  00  10 253 246 227  44  10  99
;; yellow    #b58900  3/3 yellow   136 #af8700 60  10  65 181 137   0  45 100  71
;; orange    #cb4b16  9/3 brred    166 #d75f00 50  50  55 203  75  22  18  89  80
;; red       #dc322f  1/1 red      160 #d70000 50  65  45 220  50  47   1  79  86
;; magenta   #d33682  5/5 magenta  125 #af005f 50  65 -05 211  54 130 331  74  83
;; violet    #6c71c4 13/5 brmagenta 61 #5f5faf 50  15 -45 108 113 196 237  45  77
;; blue      #268bd2  4/4 blue      33 #0087ff 55 -10 -45  38 139 210 205  82  82
;; cyan      #2aa198  6/6 cyan      37 #00afaf 60 -35 -05  42 161 152 175  74  63
;; green     #859900  2/2 green     64 #5f8700 60 -20  65 133 153   0  68 100  60

(a:define-constant +base03+  "#002b36" :test 'equal)
(a:define-constant +base02+  "#073642" :test 'equal)
(a:define-constant +base01+  "#586e75" :test 'equal)
(a:define-constant +base00+  "#657b83" :test 'equal)
(a:define-constant +base0+   "#839496" :test 'equal)
(a:define-constant +base1+   "#93a1a1" :test 'equal)
(a:define-constant +base2+   "#eee8d5" :test 'equal)
(a:define-constant +base3+   "#fdf6e3" :test 'equal)
(a:define-constant +yellow+  "#b58900" :test 'equal)
(a:define-constant +orange+  "#cb4b16" :test 'equal)
(a:define-constant +red+     "#dc322f" :test 'equal)
(a:define-constant +magenta+ "#d33682" :test 'equal)
(a:define-constant +violet+  "#6c71c4" :test 'equal)
(a:define-constant +blue+    "#268bd2" :test 'equal)
(a:define-constant +cyan+    "#2aa198" :test 'equal)
(a:define-constant +green+   "#859900" :test 'equal)

(defun setup-solarized-dark ()
  "Configure StumpWM colors for Solarized Dark."
  (swm:set-fg-color +base0+)
  (swm:set-bg-color +base03+)
  (swm:set-border-color +base1+)
  (swm:set-focus-color +base3+)
  (swm:set-unfocus-color +base1+)

  (setf swm:*mode-line-foreground-color* +base0+
        swm:*mode-line-background-color* +base03+
        swm:*mode-line-border-color* +base1+)

  ;; Set *colors*
  ;; ("bg" "red" "green" "yellow" "blue" "magenta" "cyan" "fg" "white" "black")
  (setf swm:*colors* (list +base0+
                           +red+ +green+ +yellow+ +blue+ +magenta+ +cyan+
                           +base03+
                           "#ffffff" "#000000"))
  (swm:update-color-map (swm:current-screen)))

(defun setup-font ()
  (clx-truetype:cache-fonts)
  (swm:set-font (apply #'make-instance 'clx-truetype:font vars:*font*)))
