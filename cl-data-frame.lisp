;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:cl-data-frame
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:let-plus
   #:cl-slice
   #:cl-slice-dev)
  (:export
   ;; data frame
   ))

(cl:in-package #:cl-data-frame)



;;; Ordered keys provide a mapping from column keys (symbols) to nonnegative
;;; integers.  They are used internally and the corresponding interface is
;;; NOT EXPORTED.

(defstruct (ordered-keys (:copier nil))
  "Representation of ordered keys.  KEYS contains a list of symbols (the
keys), TABLE is a hash table for quicker lookup.  Either one could be
regenerated from the other, this is maintained as an invariant."
  (keys nil :type list :read-only t)
  (table (make-hash-table :test #'eq) :type hash-table :read-only t))

(define-condition duplicate-key (error)
  ((key :initarg :key))
  (:documentation "Duplicate key.")
  (:report (lambda (condition stream)
             (format stream "Duplicate key ~A." (slot-value condition 'key)))))

(define-condition key-not-found (error)
  ((key :initarg :key)
   (keys :initarg :keys))
  (:documentation "Key not found.")
  (:report (lambda (condition stream)
             (format stream "Key ~A not found, valid keys are ~A."
                     (slot-value condition 'key)
                     (slot-value condition 'keys)))))

(defun key-list (ordered-keys)
  "List of all keys."
  (ordered-keys-keys ordered-keys))

(defun key-index (ordered-keys key)
  "Return the index for KEY."
  (let+ (((&values index present?)
          (gethash key (ordered-keys-table ordered-keys))))
    (unless present?
      (error 'key-not-found :key key :keys (key-list ordered-keys)))
    index))

(defmethod print-object ((ordered-keys ordered-keys) stream)
  (print-unreadable-object (ordered-keys stream :type t)
    (format stream "~{~a~^, ~}" (key-list ordered-keys))))

(defun add-key! (ordered-keys key)
  "Modify ORDERED-KEYS by adding KEY."
  (check-type key symbol)
  (let+ (((&structure ordered-keys- keys table) ordered-keys)
         ((&values &ign present?) (gethash key table)))
    (when present?
      (error 'duplicate-key :key key))
    (setf (gethash key table) (hash-table-count table)))
  ordered-keys)

(defun ordered-keys (keys)
  "Create an ORDERED-KEYS object from KEYS (a sequence)."
  (aprog1 (make-ordered-keys)
    (map nil (curry #'add-key! it) keys)))

(defun copy-ordered-keys (ordered-keys)
  (let+ (((&structure ordered-keys- keys table) ordered-keys))
    (make-ordered-keys :keys (copy-list keys)
                       :table (copy-hash-table table))))

(defun add-keys (ordered-keys &rest keys)
  (aprog1 (copy-ordered-keys ordered-keys)
    (mapc (curry #'add-key! it) keys)))

;;; implementation of SLICE for ORDERED-KEYS

(defmethod axis-dimension ((axis ordered-keys))
  (hash-table-count (ordered-keys-table axis)))

(defmethod canonical-representation ((axis ordered-keys) (slice symbol))
  (key-index axis slice))

(defmethod slice ((ordered-keys ordered-keys) &rest slices)
  (let+ (((slice) slices))
    (ordered-keys
     (slice (key-list ordered-keys)
            (canonical-representation ordered-keys slice)))))



(defgeneric column-length (column)
  (:documentation "Return the length of column.")
  (:method ((column vector))
    (length column)))

(defclass data-frame ()
  ((ordered-keys
    :initarg :ordered-keys
    :type ordered-keys)
   (columns
    :initarg :columns
    :type vector
    :reader columns-vector)))

(defun data-frame-length (data-frame)
  "Length of DATA-FRAME (number of rows)."
  (column-length (aref (columns-vector data-frame) 0)))

(defun column (data-frame key)
  "Return the column of DATA-FRAME corresponding to KEYS."
  (let+ (((&slots-r/o ordered-keys columns) data-frame))
    (aref columns (key-index ordered-keys key))))

(defun column-keys (data-frame)
  (key-list (slot-value data-frame 'ordered-keys)))

(defun columns-alist (data-frame)
  (map 'list #'cons (column-keys data-frame) (columns-vector data-frame)))

(defun columns-plist (data-frame)
  (alist-plist (columns-alist data-frame)))

(defun make-data-frame (ordered-keys columns)
  "Create a data frame from ORDERED-KEYS and COLUMNS (can be any kind of
sequence).  For internal use."
  (make-instance 'data-frame
                 :ordered-keys ordered-keys
                 :columns (make-array (length columns)
                                      :adjustable t
                                      :initial-contents columns)))

(defun data-frame (&rest keys-and-columns-plist)
  (assert keys-and-columns-plist () "Can't create an empty data frame.")
  (let* ((alist (plist-alist keys-and-columns-plist))
         (columns (mapcar #'cdr alist))
         (length (column-length (car columns))))
    (assert (every (lambda (column)
                     (= length (column-length column)))
                   (cdr columns)))
    (make-data-frame (ordered-keys (mapcar #'car alist))
                     columns)))

(defun copy-data-frame (data-frame)
  (let+ (((&slots-r/o ordered-keys columns) data-frame))
    (make-data-frame (copy-ordered-keys ordered-keys)
                     columns)))

(defun add-column! (data-frame key column)
  "Modify DATA-FRAME by adding COLUMN with KEY.  Return DATA-FRAME."
  (let+ (((&slots ordered-keys columns) data-frame))
    (assert (= (column-length columns) (data-frame-length data-frame)))
    (add-key! ordered-keys key)
    (vector-push-extend column columns))
  data-frame)

(defun add-columns! (data-frame &rest key-and-column-plist)
  "Modify DATA-FRAME by adding columns with keys (specified as a plist.
Return DATA-FRAME."
  (mapc (lambda+ ((key . column))
          (add-column! data-frame key column))
        (plist-alist key-and-column-plist))
  data-frame)

(defun add-columns (data-frame &rest key-and-column-plist)
  (aprog1 (copy-data-frame data-frame)
    (apply #'add-columns data-frame key-and-column-plist)))

(defmethod slice ((data-frame data-frame) &rest slices)
  (let+ (((row-slice &optional (column-slice t)) slices)
         ((&slots-r/o ordered-keys columns) data-frame)
         (column-slice (canonical-representation ordered-keys column-slice))
         (columns (slice columns column-slice))
         ((&flet slice-column (column)
            (slice column row-slice))))
    (if (singleton-representation? column-slice)
        (slice-column columns)
        (make-data-frame (slice ordered-keys column-slice)
                         (map 'vector #'slice-column columns)))))

;;; TODO: (setf slice)

(defun map-rows (data-frame keys function &key (element-type t))
  (let ((columns (map 'list (curry #'column data-frame) keys))
        (length (data-frame-length data-frame)))
    (aprog1 (make-array length :element-type element-type)
      (dotimes (index length)
        (setf (aref it index)
              (apply function
                     (mapcar (lambda (column)
                               (ref column index))
                             columns)))))))

(defun select-rows (data-frame keys function)
  (map-rows data-frame keys (compose (lambda (flag)
                                       (if flag 1 0))
                                     function)
            :element-type 'bit-vector))

(defun process-bindings (bindings)
  "Return forms for variables and keys as two values, for use in macros of
this library.  BINDINGS is a list of (VARIABLE &optional KEY) forms, where
VARIABLE is a symbol and KEY is evaluated."
  (let ((alist (mapcar (lambda+ ((variable
                                  &optional (key (make-keyword variable))))
                         (check-type variable symbol)
                         (cons variable key))
                       bindings)))
    (values (mapcar #'car alist)
            `(list ,@(mapcar #'cdr alist)))))

(defun keys-and-lambda-from-bindings (bindings body)
  "Process bindings and return a form that can be spliced into the place of
KEYS and FUNCTION (using BODY) in functions that map rows."
  (let+ (((&values variables keys) (process-bindings bindings)))
    `(,keys (lambda ,variables ,@body))))

(defmacro with-map-rows ((data-frame &key (element-type t)) bindings
                         &body body)
  `(map-rows ,data-frame
             ,@(keys-and-lambda-from-bindings bindings body)
             :element-type ,element-type))

(defmacro with-select-rows ((data-frame) bindings &body body)
  `(select-rows ,data-frame
                ,@(keys-and-lambda-from-bindings bindings body)))

(defmacro define-map-add-function-and-macro (blurb (function function-used)
                                             macro)
  "Macro for defining functions that map and add columns.  BLURB is used in
the docstring, FUNCTION is defined using FUNCTION-USED, and MACRO is the
corresponding macro."
  `(progn
     (defun ,function (data-frame keys function result-key
                       &key (element-type t))
       ,(format nil
"Map columns of DATA-FRAME and add the resulting column (with the given
ELEMENT-TYPE), designated by RESULT-KEY.  ~A

KEYS selects columns, the rows of which are passed on to FUNCTION."
                blurb)
       (,function-used data-frame result-key
                       (map-rows data-frame keys function
                                 :element-type element-type)))
     (defmacro ,macro ((data-frame key &key (element-type t))
                       bindings &body body)
       ,(format nil
"Map columns of DATA-FRAME and add the resulting column (with the given
ELEMENT-TYPE), designated by KEY.  ~A

BINDINGS is a list of (VARIABLE KEY) forms, binding the values in each row to
the VARIABLEs for the columns designated by KEYs."
                blurb)
       `(,',function ,data-frame
                     ,@(keys-and-lambda-from-bindings bindings body)
                     ,key
                     :element-type ,element-type))))

(define-map-add-function-and-macro
    "Return a new data-frame."
    (map-rows-and-add add-columns)
    with-map-rows-and-add)

(define-map-add-function-and-macro
    "Modify (and also return) DATA-FRAME."
    (map-rows-and-add! add-column!)
    with-map-rows-and-add!)
