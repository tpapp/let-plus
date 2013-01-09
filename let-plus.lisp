;;;; let-plus.lisp

(in-package #:let-plus)

;;; LET+ recognizes three general kinds of syntax for accessing elements in
;;; some structure (in the abstract sense):
;;;
;;; 1. "slots", of the form (VARIABLE &optional (SLOT VARIABLE)) SLOT is used
;;;    in the general sense, it can also be an accessor.  This is similar to
;;;    the syntax of WITH-SLOTS etc.
;;;
;;; 2. "entries", of the form (VARIABLE &optional (KEY VARIABLE) DEFAULT),
;;;    which allows a default value.  This is used for hash tables, property
;;;    lists, etc.  If KEY is NIL, VARIABLE is used instead, if another
;;;    symbol, it is quoted.
;;;
;;; 3. array-like reference (VARIABLE &rest SUBSCRIPTS).  This is used for
;;;    array elements.
;;;
;;; If a single symbol is given, it is used as a variable for entries and
;;; slots.

;;; Ignored variables
;;;
;;; The preferred method is expanding into LET+ forms which handle ignored
;;; values automatically -- LET+ just ignores these variables.  Use
;;; REPLACE-IGNORED only when this is not feasible or desirable (eg using
;;; destructuring provided by CL).

(defun ignored? (symbol)
  "Return a boolean determining if a variable is to be ignored.

NOTE: It is unlikely that you need to used this function, see the note above its definition."
  (eq symbol '&ign))

(defun replace-ignored (tree)
  "Replace ignored variables in TREE with a gensym, return a list of these as the second value.

NOTE: It is unlikely that you need to used this function, see the note above its definition."
  (let (ignored)
    (labels ((traverse (tree)
               (if (atom tree)
                   (if (ignored? tree)
                       (aprog1 (gensym)
                         (push it ignored))
                       tree)
                   (cons (traverse (car tree))
                         (awhen (cdr tree) (traverse it))))))
      (values (traverse tree) (nreverse ignored)))))

;;; LET+ uses generic functions for expansion.  They are dispatched on the
;;; FORM, and further on the first element if it is a list.  LET+-EXPANSION
;;; should wrap BODY in the desired forms, implementing the expansion.  The
;;; recursive expansion of multiple forms is done by the LET+ macro.
;;;
;;; LET+ forms start with & (except for those expanding into LET(*) and
;;; DESTRUCTURING-BIND), although this convention is not enfored.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun &-symbol? (symbol)
    "Test whether the symbol's name starts with a & character."
    (char= (aref (symbol-name symbol) 0) #\&)))

(defgeneric let+-expansion (form value body)
  (:documentation "Return an expansion for a LET+ form.")
  (:method (form value body)
    (declare (ignore value body))
    (error "LET+ could not recognize ~A." form))
  (:method ((variable null) value body)
    `(destructuring-bind nil ,value
       ,@body))
  (:method ((variable symbol) value body)
    (cond
      ((ignored? variable) `(progn ,@body))
      ((&-symbol? variable)
       (warn "Possibly left out one level of nesting in LET+ form (~A ~A)."
             variable value))
      (t `(let ((,variable ,@(when value `(,value))))
            ,@body))))
  (:method ((form list) value body)
    (let+-expansion-for-list (first form) (rest form) value body)))

(defgeneric let+-expansion-for-list (first rest value body)
  (:documentation "LET+-EXPANSION calls this for lists, see the latter for semantics of returned values.")
  (:method (first rest value body)
    ;; forms not recognized as anything else are destructured
    (when (and (symbolp first) (not (ignored? first)) (&-symbol? first)
               (not (find first lambda-list-keywords)))
      (warn "~A looks like a LET+ keyword, but it has no expansion method defined.  Treating it as a lambda list." first))
    (let ((form (cons first rest)))
      (multiple-value-bind (form ignored) (replace-ignored form)
        `(destructuring-bind ,form ,value
           (declare (ignore ,@ignored))
           ,@body)))))

(defmacro let+ (bindings &body body)
  "Destructuring bindings.  See the documentation of the LET-PLUS library.  Most accepted forms start with &."
  (labels ((expand (bindings)
             (destructuring-bind (binding &rest other-bindings) bindings
               (destructuring-bind (form &optional value)
                   (ensure-list binding)
                 (let+-expansion form value (aif other-bindings
                                                 (list (expand it))
                                                 body))))))
    (if bindings
        (expand bindings)
        `(progn ,@body))))

(defmacro define-let+-expansion ((name arguments &key
                                       (value-var 'value)
                                       (body-var 'body)
                                       (uses-value? t)
                                       (once-only? uses-value?))
                                 &body body)
  "Define an expansion for LET+ forms which are lists, starting with NAME.  ARGUMENTS is destructured if a list.  A placeholder macro is defined with NAME, using DOCSTRING and ARGUMENTS.  The value form is bound to VALUE-VAR (wrapped in ONCE-ONLY when ONCE-ONLY?), while the body is bound to BODY-VAR.  USES-VALUE? determines if the form uses a value, and generates the appropriate checks."
  (let ((arguments-var (gensym "ARGUMENTS"))
        (arguments (if (listp arguments)
                       arguments
                       `(&rest ,arguments)))
        (whole (gensym "WHOLE")))
    (multiple-value-bind (remaining-forms declarations docstring)
        (parse-body body :documentation t)
      (sunless docstring (setf it (format nil "LET+ form ~A." name)))
      `(progn
         (defmacro ,name (&whole ,whole ,@arguments)
           ,docstring
           (declare (ignore
                     ,@(remove-if (lambda (symbol)
                                    (or (not symbol)
                                        (not (symbolp symbol))
                                        (keywordp symbol)
                                        (find symbol lambda-list-keywords)
                                        (&-symbol? symbol)))
                                  (flatten arguments))))
           ,@declarations
           ,whole)
         (defmethod let+-expansion-for-list ((first (eql ',name))
                                             ,arguments-var ,value-var
                                             ,body-var)
           ,(if uses-value?
                `(assert ,value-var () "Missing value form in ~A." ',name)
                `(assert (not ,value-var) ()
                         "~A forms don't take a value." ',name))
           ,(let ((core `(destructuring-bind ,arguments ,arguments-var
                           ,@declarations
                           ,@remaining-forms)))
              (if once-only? ; basically once-only, with ignorable value
                  (with-unique-names (value-once-var)
                    `(let ((,value-once-var (gensym "VALUE")))
                       `(let ((,,value-once-var ,,value-var))
                          (declare (ignorable ,,value-once-var))
                          ,(let ((,value-var ,value-once-var))
                             ,core))))
                  core)))))))

;;; Definitions for particular LET+ forms.
;;;
;;; When both read only and read/write forms make sense, the former should
;;; have the suffix -r/o and the latter should be without the suffix in order
;;; to maintain a consistent naming scheme.

;;; helper functions

(defun expand-slot-forms (slots accessor-generator)
  "Return a list of expanded bindings, calling (ACCESSOR-GENERATOR KEY)"
  (let (bindings)
    (loop for entry :in slots do
      (destructuring-bind (variable &optional (key variable))
          (ensure-list entry)
        (when variable
          (push `(,variable ,(funcall accessor-generator key)) bindings))))
    (nreverse bindings)))


(defun expand-entry-forms (entries accessor-generator)
  "Return a list of expanded bindings from ENTRIES, calling (ACESSOR-GENERATOR KEY DEFAULT).  Each entry is (VARIABLE &OPTIONAL KEY DEFAULT).  When KEY is NIL, VARIABLE is used."
  (mapcar (lambda (entry)
            (destructuring-bind (variable &optional key default)
                (ensure-list entry)
              `(,variable ,(funcall accessor-generator
                                    (typecase key
                                      (null `',variable)
                                      (symbol `',key)
                                      (t key))
                                    default))))
          entries))

(defun expand-array-elements (value array-elements &optional (accessor 'aref))
  "Expand a list of (BINDING &REST SUBSCRIPTS) forms to a list of bindings of the form (ACCESSOR VALUE SUBSCRIPTS)."
  (mapcar (lambda (array-element)
            `(,(first array-element)
               (,accessor ,value ,@(rest array-element))))
          array-elements))

(define-let+-expansion (&accessors accessors)
  "LET+ form, similar to WITH-ACCESSORS."
  `(symbol-macrolet ,(expand-slot-forms accessors (lambda (accessor)
                                                    `(,accessor ,value)))
     ,@body))

(define-let+-expansion (&accessors-r/o accessors)
  "LET+ form, similar to WITH-ACCESSORS, but read-only."
  `(let+ ,(expand-slot-forms accessors (lambda (accessor)
                                         `(,accessor ,value)))
     ,@body))

(define-let+-expansion (&slots slots :once-only? nil)
  "LET+ form, similar to WITH-SLOTS."
  `(with-slots ,slots ,value
     ,@body))

(define-let+-expansion (&slots-r/o slots)
  "LET+ form, similar to WITH-SLOTS but read-only."
  `(let+ ,(expand-slot-forms slots
                             (lambda (slot) `(slot-value ,value ',slot)))
     ,@body))

(define-let+-expansion (&structure (conc-name &rest slots))
  "LET+ form for slots of a structure, with accessors generated using CONC-NAME."
  (check-type conc-name symbol)
  `(symbol-macrolet
       ,(expand-slot-forms slots
                           (lambda (slot) `(,(symbolicate conc-name slot)
                                            ,value)))
     ,@body))

(define-let+-expansion (&structure-r/o (conc-name &rest slots))
  "LET+ form for slots of a structure, with accessors generated using CONC-NAME.  Read-only version."
  (check-type conc-name symbol)
  `(let+ ,(expand-slot-forms slots
                             (lambda (slot)
                               `(,(symbolicate conc-name slot) ,value)))
     ,@body))

(define-let+-expansion (&values values :once-only? nil)
  "LET+ form for multiple values."
  (let ((values-and-temps (mapcar (lambda (v) (list v (gensym))) values)))
    `(multiple-value-bind ,(mapcar #'second values-and-temps) ,value
       (declare (ignore ,@(loop for (v g) in values-and-temps
                                when (ignored? v)
                                  collect g)))
       (let+ ,(remove-if (compose #'ignored? #'car) values-and-temps)
         ,@body))))

(defmethod let+-expansion ((array array) value body)
  "LET+ expansion for mapping array elements to variables."
  (let (bindings
        (value-var (gensym "VALUE")))
    (dotimes (row-major-index (array-total-size array))
      (let ((variable (row-major-aref array row-major-index)))
        (unless (ignored? variable)
          (push `(,variable
                  (row-major-aref ,value-var ,row-major-index))
                bindings))))
    `(let ((,value-var ,value))
       (assert (equal (array-dimensions ,value-var)
                      ',(array-dimensions array)))
       (let+ ,(nreverse bindings)
         ,@body))))



(define-let+-expansion (&array-elements array-elements)
  "LET+ form, mapping (variable &rest subscripts) specifications to array-elements.  VARIABLE is an accessor, which can be used for reading and writing array elements."
  `(symbol-macrolet ,(expand-array-elements value array-elements)
     ,@body))

(define-let+-expansion (&array-elements-r/o array-elements)
  "LET+ form, mapping (variable &rest subscripts) specifications to array-elements.  Read-only accessor, values assigned to VARIABLEs."
  (once-only (value)
    `(let+ ,(expand-array-elements value array-elements)
       ,@body)))

(define-let+-expansion (&flet (function-name lambda-list
                                             &body function-body)
                           :uses-value? nil)
  "LET+ form for function definitions.  Expands into an FLET."
  `(flet ((,function-name ,lambda-list ,@function-body))
     ,@body))

(define-let+-expansion (&labels (function-name lambda-list
                                               &body function-body)
                           :uses-value? nil)
  "LET+ form for function definitions.  Expands into an LABELS, thus allowing recursive functions."
  `(labels ((,function-name ,lambda-list ,@function-body))
     ,@body))

(define-let+-expansion (&macrolet (macro-name lambda-list  &body macro-body)
                           :uses-value? nil)
  "LET+ form for local macro definitions.  Expands into an MACROLET."
  `(macrolet ((,macro-name ,lambda-list ,@macro-body))
     ,@body))

(define-let+-expansion (&symbol-macrolet (symbol expansion) :uses-value? nil)
  "LET+ form for local symbol macros.  Expands into a SYMBOL-MACROLET."
  `(symbol-macrolet ((,symbol ,expansion))
     ,@body))

(define-let+-expansion (&plist entries)
  "LET+ form for property lists.  Each entry is (variable &optional key default)."
  `(symbol-macrolet
       ,(expand-entry-forms entries
                            (lambda (key default)
                              `(getf ,value ,key ,default)))
     ,@body))

(define-let+-expansion (&plist-r/o entries)
  "LET+ form for property lists, read only version.  Each entry is (variable &optional key default)."
  `(let* ,(expand-entry-forms entries
                              (lambda (key default)
                                `(getf ,value ,key ,default)))
     ,@body))

(define-let+-expansion (&hash-table entries)
  "LET+ form for hash tables.  Each entry is (variable &optional key default)."
  `(symbol-macrolet
       ,(expand-entry-forms entries
                            (lambda (key default)
                              `(gethash ,key ,value ,default)))
     ,@body))

(define-let+-expansion (&hash-table-r/o entries)
  "LET+ form for hash tables.  Each entry is (variable &optional key default).  Read only version."
  `(let+ ,(expand-entry-forms entries
                              (lambda (key default) `(gethash ,key ,value ,default)))
     ,@body))
