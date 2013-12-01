;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(defpackage #:let-plus-tests
  (:use #:cl #:alexandria #:lift #:let-plus)
  (:export #:run))

(in-package #:let-plus-tests)

(deftestsuite let-plus-tests () ())

(defun run ()
  "Run all the tests for LET-PLUS."
  (run-tests :suite 'let-plus-tests))

(defmacro test-r/o-and-r/w (init-form bindings r/o-form r/w-form
                            &key (add 7)
                            (modified-variable (caar bindings)))
  "Macro for autogenerating tests for testing read-only and read-write access.
INIT-FORM is evaluated and bound to a value, which is then bound using LET+
with r/o-form (for read only access) and r/w-form for (read and write access).
Values are compared to bindings (which are of the form (VARIABLE VALUE), where
value should be numeric.  MODIFIED-VARIABLE is modified, by adding ADD and
checked for consistency (r/o form should not modify value, while r/w form
should)."
  (with-unique-names (object)
    (once-only (add)
      `(let ((,object ,init-form))
         (let+ ((,r/o-form ,object))
           ,@(loop for (var value) in bindings
                   collect `(ensure-same ,var ,value))
           (incf ,modified-variable ,add))
         (let+ ((,r/w-form ,object))
           ,@(loop for (var value) in bindings
                   collect `(ensure-same ,var ,value))
           (incf ,modified-variable ,add))
         (let+ ((,r/o-form ,object))
           (ensure-same ,(caar bindings) (+ ,add ,(cadar bindings))))))))

(addtest (let-plus-tests)
  test-let*-compatibility
  (let+ (a
         (b 1)
         (c (+ b 3)))
    (ensure-same a nil)
    (ensure-same b 1)
    (ensure-same c 4)))

(addtest (let-plus-tests)
  test-unrecognized-form
  (ensure-warning (macroexpand '(let+ (((&does-not-exist a b) '(1 2 3)))))))

(defclass foo ()
  ((bar :accessor bar :initarg :bar)
   (baz :accessor baz-acc :initarg :baz))
  (:documentation "Class for testing purposes."))

(addtest (let-plus-tests)
  test-slots
  (test-r/o-and-r/w (make-instance 'foo :bar 1 :baz 2)
                    ((bar 1) (baz2 2))
                    (&slots-r/o bar (baz2 baz))
                    (&slots bar (baz2 baz))))

(addtest (let-plus-tests)
  test-accessors
  (test-r/o-and-r/w (make-instance 'foo :bar 1 :baz 2)
                    ((bar 1) (baz 2))
                    (&accessors-r/o bar (baz baz-acc))
                    (&accessors bar (baz baz-acc))))

(defstruct foo2
  "Structure for testing purposes."
  bar baz)

(addtest (let-plus-tests)
  test-structure
  (test-r/o-and-r/w (make-foo2 :bar 1 :baz 2)
                    ((bar 1) (baz 2))
                    (&structure-r/o foo2- bar (baz baz))
                    (&structure foo2- bar (baz baz))))

(addtest (let-plus-tests)
  test-values
  (let+ (((&values a &ign c) (values 1 2 3)))
    (ensure-same a 1)
    (ensure-same c 3)))

(addtest (let-plus-tests)
  test-values-recursive
  (let+ (((&values (a b) &ign c) (values '(1 4) 2 3)))
    (ensure-same a 1)
    (ensure-same b 4)
    (ensure-same c 3)))

(addtest (let-plus-tests)
  test-array
  (let+ ((#2A((a &ign) (b c)) #2A((1 2) (3 4))))
    (ensure-same a 1)
    (ensure-same b 3)
    (ensure-same c 4))
  (ensure-error (let+ ((#(a b c) #(1 2))))))

(addtest (let-plus-tests)
  test-array-elements
  (test-r/o-and-r/w (make-array '(2 2) :initial-contents '((1 2) (3 4)))
                    ((a 1) (b 3) (c 4))
                    (&array-elements-r/o (a 0 0)
                                         (b 1 0)
                                         (c 1 1))
                    (&array-elements (a 0 0)
                                     (b 1 0)
                                     (c 1 1))))

(addtest (let-plus-tests)
  test-flet
  (let+ (((&flet add1 (x)
            (1+ x)) ))
    (ensure-same (add1 2) 3)))

(addtest (let-plus-tests)
  test-labels
  (let+ (((&labels my-factorial (x)
            (if (<= 2 x)
                (* x (my-factorial (1- x)))
                1))))
    (ensure-same (my-factorial 4) 24)))

(addtest (let-plus-tests)
  test-plist
  (test-r/o-and-r/w (list 'a 1 :b 2 'c '3)
                    ((a 1) (b 2) (c 3) (d 4))
                    (&plist-r/o a (b :b) (c nil) (d nil 4))
                    (&plist a (b :b) (c nil) (d nil 4))))

(addtest (let-plus-tests)
  test-hash-table
  (test-r/o-and-r/w (let ((table (make-hash-table)))
                      (setf (gethash 'a table) 1
                            (gethash :b table) 2
                            (gethash 'c table) 3)
                      table)
                    ((a 1) (b 2) (c 3) (d 4))
                    (&hash-table-r/o a (b :b) (c nil) (d nil 4))
                    (&hash-table a (b :b) (c nil) (d nil 4))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct ab
      "A structure for testing defstruct+."
    a b)
  (define-structure-let+ (ab) a b))

(addtest (let-plus-tests)
  test-defstruct+
  (test-r/o-and-r/w (make-ab :a 3 :b 7)
                    ((aa 3) (bb 7))
                    (&ab-r/o aa bb)
                    (&ab aa bb)))

(addtest (let-plus-tests)
  test-fwrap
  (let+ (((&fwrap add) (lambda (a b &key (c 0)) (+ a b c))))
    (ensure-same (add 1 2) 3)
    (ensure-same (add 1 2 :c 3) 6)))

(addtest (let-plus-tests)
  test-flet+
  (ensure-same (let+ (((&flet+ add3 (#(a b c))
                         (+ a b c))))
                 (add3 #(1 2 3)))
               6))

(addtest (let-plus-tests)
  test-labels+
  (let+ (((&labels+ foo ((a . b))
            (if a
                (foo (cons (cdr a) (cons (car a) b)))
                b))))
    (ensure-same (foo '((1 2 3) . nil)) '(3 2 1))))

(addtest (let-plus-tests)
  test-defun+
  (defun+ foo ((a . b))
    "bar"
    (+ a b))
  (ensure-same (foo '(1 . 2)) 3)
  (ensure-same (documentation 'foo 'function) "bar" :test #'string=)
  (let ((expansion (macroexpand-1
                    '(defun+ foo ((a . b))
                      "foo docstring"
                      (declare (type integer a b))
                      (+ a b)))))
    (ensure-same 'defun (first expansion))
    (ensure-same 'foo (second expansion))
    (ensure-same "foo docstring" (fourth expansion))
    (let ((let+-expansion (fifth expansion)))
      (ensure-same 'let+ (first let+-expansion))
      (ensure-same '(declare (type integer a b)) (third let+-expansion))
      (ensure-same '(+ a b) (fourth let+-expansion)))))

(addtest (let-plus-tests)
  test-&warning
  (ensure-warning
    (macroexpand '(let+ ((&foo 1)) &foo)))
  (ensure-warning
    (macroexpand '(let+ ((&foo)) &foo)))
  (ensure-warning
    (macroexpand '(let+ (&foo) &foo))))

(addtest (let-plus-tests)
  test-empty-let+
  (ensure-same (let+ nil 1) 1 :test #'eql))

(addtest (let-plus-tests)
  test-recursive-let+
  (ensure-same (let+ ((#((a . b)) (vector (cons 1 2))))
                 (values a b))
               (values 1 2))
  ;; (ensure-same (let+ (((#(a) #(b c)) (cons #(1) #(2 3))))
  ;;                (values a b c))
  ;;              (values 1 2 3))
  )

(addtest (let-plus-tests)
  test-complex
  (let ((c #C(3 5)))
    (let+ (((&complex x y) c))
      (ensure-same x 3)
      (ensure-same y 5))))

(addtest (let-plus-tests)
  test-nil
  (ensure (let+ ((() '())) t))
  (ensure-error (let+ ((() '(1)))))
  ;; (ensure-error (let+ ((() 1))))
)
