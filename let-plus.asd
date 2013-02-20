;;;; let-plus.asd

(asdf:defsystem #:let-plus
  :description "Destructuring extension of LET*."
  :author "Tamas K. Papp <tkpapp@gmail.com>."
  :license "Boost Software License - Version 1.0"
  :version "0.2"
  :serial t
  :components ((:file "package")
               (:file "let-plus")
               (:file "extensions"))
  :depends-on (#:alexandria
               #:anaphora))

(defmethod perform ((op test-op) (sys (eql (find-system '#:let-plus))))
  (operate 'test-op '#:let-plus-tests))

(asdf:defsystem #:let-plus-tests
  :description "Tests for the LET-PLUS library."
  :author "Tamas K. Papp <tkpapp@gmail.com>."
  :license "Same as LET-PLUS -- this is part of the latter."
  :serial t
  :components ((:file "tests"))
  :depends-on (#:lift
               #:let-plus))

(defmethod perform ((op test-op) (sys (eql (find-system '#:let-plus-tests))))
  (operate 'load-op '#:let-plus-tests)
  (funcall (find-symbol (string '#:run) '#:let-plus-tests)))
