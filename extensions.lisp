;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:let-plus)

(defun destructured-lambda-list-forms (lambda-list body)
  "Return a list that can be spliced into function definitions (eg DEFUN, LAMBDA, FLET, LABELS).

The list starts with a lambda list, and is followed by a docstring (when provided), then a LET+ form that wraps declarations (when provided) and BODY.

Used internally, not exported."
  (let+ (((&values body declarations documentation)
          (parse-body body :documentation t))
         ((&values arguments bindings ignores)
          (loop :for parameter :in lambda-list
             :for argument = (gensym)
             :collect argument :into arguments
             :if (eq parameter '&ign) :collect argument :into ignores
             :else :collect (list parameter argument) :into bindings
             :finally (return (values arguments bindings ignores)))))
    `(,arguments
      ,@(when documentation `(,documentation))
      ,@(when ignores `((declare (ignore ,@ignores))))
      (let+ ,bindings
        ,@declarations
        ,@body))))

(define-let+-expansion (&flet+ (function-name lambda-list
                                              &body function-body)
                           :uses-value? nil)
  "&FLET that destructures its arguments using LET+."
  `(let+ (((&flet ,function-name
               ,@(destructured-lambda-list-forms lambda-list function-body))))
     ,@body))

(define-let+-expansion (&labels+ (function-name lambda-list
                                                &body function-body)
                               :uses-value? nil)
  "&LABELS that destructures its arguments using LET+."
  `(let+ (((&labels ,function-name
               ,@(destructured-lambda-list-forms lambda-list function-body))))
     ,@body))

(defmacro lambda+ (lambda-list &body body)
  "LAMBDA that destructures its arguments using LET+."
  `(lambda ,@(destructured-lambda-list-forms lambda-list body)))

(defmacro defun+ (name lambda-list &body body)
  "DEFUN that destructures its arguments using LET+."
  `(defun ,name ,@(destructured-lambda-list-forms lambda-list body)))

(defmacro define-structure-let+ ((name
                                  &key (conc-name (symbolicate name #\-))
                                       (r/w (symbolicate #\& name))
                                       (r/o (symbolicate #\& name '#:-r/o)))
                                 &rest slot-names)
  "Define a LET+ expansion for accessing slots of a structure in a fixed order."
  (let ((variable-name-pairs
         (loop for slot-name in slot-names collect
               ``(,,slot-name ,',slot-name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (define-let+-expansion (,r/w (,@slot-names))
         ,(format nil "LET+ form for slots of the structure ~A." name)
         `(let+ (((&structure ,',conc-name ,,@variable-name-pairs) ,value))
            ,@body))
       (define-let+-expansion (,r/o (,@slot-names))
         ,(format nil "LET+ form for slots of the structure ~A.  Read-only."
                  name)
         `(let+ (((&structure-r/o ,',conc-name ,,@variable-name-pairs)
                  ,value))
            ,@body)))))

(define-let+-expansion (&fwrap (name))
  "Wrap closure in the local function NAME.  Calls to NAME will call the closure."
  `(let+ (((&flet ,name (&rest arguments)
             (apply ,value arguments))))
     ,@body))

(define-let+-expansion (&once-only specs :uses-value? nil)
  "Expand to (ONCE-ONLY SPECS ...)."
  `(once-only ,specs ,@body))

(define-let+-expansion (&with-gensyms names :uses-value? nil)
  "Expand to (WITH-GENSYMS NAMES ...)."
  `(with-gensyms ,names ,@body))

(define-let+-expansion (&complex (x y))
  "Access real and imaginary part of the value.  Read-only."
  `(let ((,x (realpart ,value))
         (,y (imagpart ,value)))
     ,@body))
