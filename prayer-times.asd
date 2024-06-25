(defsystem #:prayer-times
  :description "Prints next prayer time in your location"
  :version "0.0.1"
  :author "Ahmed Eid"
  :licence "MIT"
  :depends-on (#:alexandria
               #:dexador
               #:bt-semaphore
               #:cl-mpg123
               #:cl-out123
               #:yason
               #:local-time
               #:cl-ppcre)
  :components ((:file "prayer-times"))
  :build-operation "program-op"
  :build-pathname "prayer-times"
  :entry-point "prayer-times::main")
