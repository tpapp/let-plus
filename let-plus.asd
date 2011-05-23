;;;; let-plus.asd

(asdf:defsystem let-plus
  :description "Destructuring extension of LET*."
  :author "Tamas K. Papp <tkpapp@gmail.com>."
  :license "Boost Software License - Version 1.0"
  :version "0.1"
  :serial t
  :components ((:file "package")
               (:file "let-plus"))
  :depends-on (alexandria anaphora))

(asdf:defsystem let-plus-tests
  :description "Tests for the LET-PLUS library."
  :author "Tamas K. Papp <tkpapp@gmail.com>."
  :license "Same as LET-PLUS -- this is part of the latter."
  :serial t
  :components ((:file "tests"))
  :depends-on (lift let-plus))
