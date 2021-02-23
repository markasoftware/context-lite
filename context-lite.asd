;;;; Copyright 2021 Mark Polyakov
;;;; Released under the MIT license

(asdf:defsystem #:context-lite
  :description "A CLOS extension to support specializing methods on special/dynamic variables."
  :author "Mark Polyakov"
  :license  "MIT"
  :version "0.1.0"
  :depends-on (#:closer-mop)
  :components ((:file "context-lite")))

(asdf:defsystem #:context-lite/test
  :description "Unit tests for Context Lite"
  :author "Mark Polyakov"
  :license "MIT"
  :version "0.1.0"
  :depends-on (#:context-lite #:fiveam)
  :components ((:file "test")))
