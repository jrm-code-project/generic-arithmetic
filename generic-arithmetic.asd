(defsystem "generic-arithmetic"
  :description "A library for generic arithmetic operations"
  :author "Joe Marshall"
  :license "MIT"
  :version "0.1.0"
  :components ((:file "package")
               (:file "generic-arithmetic" :depends-on ("package")))
  :depends-on ("fold"))
