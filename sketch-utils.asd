(defsystem "sketch-utils"
  :description "Utilities for `sketch' library."
  :version "1.0.0"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :depends-on ("sketch" "stealth-mixin")
  :serial T
  :components ((:file "sketch-buttons")
               (:file "sketch-utils")))
