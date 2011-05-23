;;;; package.lisp

(defpackage let-plus
  (:use cl alexandria anaphora)
  (:export 
   
   ;; user interface
   
   let+ lambda+ defun+ defstruct+
   
   &accessors &accessors-r/o &slots &slots-r/o &structure &structure-r/o
   &values &array-elements &array-elements-r/o &flet &labels &plist &plist-r/o
   &hash-table &hash-table-r/o

   ;; defining new forms
   
   let+-expansion let+-expansion-for-list define-let+-expansion
   
   ))

