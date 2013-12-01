;;;; package.lisp

(defpackage #:let-plus
  (:use #:cl #:alexandria #:anaphora)
  (:export ; basic user interface
   #:let+
   #:&ign
   #:&accessors
   #:&accessors-r/o
   #:&slots
   #:&slots-r/o
   #:&structure
   #:&structure-r/o
   #:&values
   #:&array-elements
   #:&array-elements-r/o
   #:&flet #:&labels
   #:&macrolet
   #:&symbol-macrolet
   #:&plist
   #:&plist-r/o
   #:&hash-table
   #:&hash-table-r/o)
  (:export ; defining new forms
   #:let+-expansion
   #:let+-expansion-for-list
   #:define-let+-expansion)
  (:export ; extensions
   #:&flet+
   #:&labels+
   #:lambda+
   #:defun+
   #:define-structure-let+
   #:&fwrap
   #:&once-only
   #:&with-gensyms
   #:&complex))
