;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:let-plus)

(defun destructured-lambda-list-forms (lambda-list body)
  "Return (list ARGUMENTS BODY), ie the arguments for the lambda list to be
destructured and the destructing form, wrapping the original body."
  (let ((arguments (loop repeat (length lambda-list) collect (gensym))))
    (list arguments
          `(let+ ,(mapcar #'list lambda-list arguments)
             ,@body))))
                      
(define-let+-expansion (&flet+ (function-name lambda-list
                                              &body function-body)
                               :once-only? nil)
  "&FLET that destructures its arguments using LET+."
  `(let+ (((&flet ,function-name
               ,@(destructured-lambda-list-forms lambda-list function-body))))
     ,@body))

(define-let+-expansion (&labels+ (function-name lambda-list
                                                &body function-body)
                               :once-only? nil)
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
  "Define a LET+ expansion for accessing slots of a structure in a fixed
order."
  (let ((variable-name-pairs
         (loop for slot-name in slot-names collect
               ``(,,slot-name ,',slot-name))))
    `(progn
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
  "Wrap closure in the local function NAME.  Calls to name will call the
closure"
  `(let+ (((&flet ,name (&rest arguments)
             (apply ,value arguments))))
     ,@body))
