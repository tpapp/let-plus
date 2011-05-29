;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(in-package #:let-plus)

(defmacro lambda+ (lambda-list &body body)
  "LAMBDA that destructures its arguments using LET+."
  (let ((arguments (loop repeat (length lambda-list) collect (gensym))))
    `(lambda ,arguments
       (let+ ,(mapcar #'list lambda-list arguments)
         ,@body))))

(defmacro defun+ (name lambda-list &body body)
  "DEFUN that destructures its arguments using LET+."
  (let ((arguments (loop repeat (length lambda-list) collect (gensym))))
    `(defun ,name ,arguments
       (let+ ,(mapcar #'list lambda-list arguments)
         ,@body))))

(defmacro defstruct+ (name-and-options &rest slot-descriptions)
  "Define a structure with let+ forms for accessing and reading slots.  Syntax
is the same as that of DEFSTRUCT, except that a (:let+ PREFIX PREFIX-R/O)
option is accepted.  The defaults are &NAME and &NAME-R/O."
  ;; implementation note: this macro is very convoluted, most of it comes from
  ;; accomodating the syntax of defstruct
  (let+ (((name &rest options) (ensure-list name-and-options))
         ((&flet find-option (option &optional (options options))
            (find option options :key #'car)))
         (conc-name (aif (second 
                          (find-option :conc-name
                                       (mapcar #'ensure-list options)))
                         it
                         (concat-symbols name '#:-)))
         (let+-options (awhen (find-option :let+)
                         (setf options (remove :let+ options
                                               :key #'car))
                         it))
         ((r/w-prefix
           &optional (r/o-prefix (concat-symbols r/w-prefix '#:-r/o)))
          (if let+-options
              (cdr let+-options)
              (list (concat-symbols '#:& name))))
         (slot-names (mapcar (compose #'first #'ensure-list)
                             (if (stringp (first slot-descriptions))
                                 (cdr slot-descriptions)
                                 slot-descriptions)))
         (variable-name-pairs
          (loop for slot-name in slot-names collect
                ``(,,slot-name ,',slot-name))))
    `(progn
       (defstruct ,name-and-options ,@slot-descriptions)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (define-let+-expansion (,r/w-prefix (,@slot-names))
           ,(format nil "LET+ form for slots of the structure ~A." name)
           `(let+ (((&structure ,',conc-name ,,@variable-name-pairs) ,value))
                  ,@body))
         (define-let+-expansion (,r/o-prefix (,@slot-names))
           ,(format nil "LET+ form for slots of the structure ~A.  Read-only."
                    name)
           `(let+ (((&structure-r/o ,',conc-name ,,@variable-name-pairs)
                    ,value))
                  ,@body))))))

(define-let+-expansion (&fwrap (name))
  "Wrap closure in the local function NAME.  Calls to name will call the
closure"
  `(let+ (((&flet ,name (&rest arguments)
             (apply ,value arguments))))
     ,@body))
