;;; -*- mode:lisp -*-

#-asdf3.3
(error "This systems needs at least ASDF 3.3.3.3 with :local-nicknames support.")

(asdf:defsystem "stumpwm.d"
  :version      "0.1.0"
  :description  "Personal config of StumpWM."
  :author       "Andriy Kmit <dev@madand.net>"
  :license      "CC0"
  :class :package-inferred-system
  :pathname "src"
  :depends-on   ("stumpwm.d/main"))
