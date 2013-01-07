;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-



(cl:defpackage #:cl-data-frame.column
  (:use #:cl
        #:alexandria
        #:anaphora
        #:let-plus)
  (:export
   #:column-length
   #:column-summary))

(cl:in-package #:cl-data-frame.column)

(defgeneric column-length (column)
  (:documentation "Return the length of column.")
  (:method ((column vector))
    (length column)))

(defstruct bit-vector-summary
  "Summary of a bit vector."
  (length 0 :type array-index :read-only t)
  (count  0 :type array-index :read-only t))

(defstruct numeric-vector-summary
  "Summary of a numeric vector."
  (length 0 :type array-index :read-only t)
  (real-count 0 :type array-index :read-only t)
  (min 0 :type real :read-only t)
  (q25 0 :type real :read-only t)
  (q50 0 :type real :read-only t)
  (q75 0 :type real :read-only t)
  (max 0 :type real :read-only t))

(defgeneric column-summary (column)
  (:documentation "Return an object that summarizes COLUMN of a DATA-FRAME.  Primarily intended for printing, not analysis, returned values should print nicely.")
  (:method ((column bit-vector))
    (make-bit-vector-summary :length (length column) :count (count 1 column)))
  (:method ((column vector))
    (let+ ((elements (loop for elt across column
                           when (realp elt)
                           collect elt))
           (#(min q25 q50 q75 max) (clnu:quantiles elements #(0 0.25 0.5 0.75 1))))
      (make-numeric-vector-summary :length (length column)
                                   :real-count (length elements)
                                   :min min
                                   :q25 q25
                                   :q50 q50
                                   :q75 q75
                                   :max max))))




(cl:defpackage #:cl-data-frame
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:let-plus
   #:cl-data-frame.column
   #:cl-slice
   #:cl-slice-dev)
  (:export
   ;; data frame
   #:duplicate-key
   #:key-not-found
   #:data-frame
   #:alist-data-frame
   #:plist-data-frame
   #:columns-vector
   #:column-length
   #:data-frame-length
   #:column
   #:data-frame-keys
   #:data-frame-alist
   #:data-frame-plist
   #:copy-data-frame
   #:add-columns
   #:add-column!
   #:add-columns!
   #:map-rows
   #:select-rows
   #:mapping-rows
   #:selecting-rows
   #:add-map-rows
   #:add-mapping-rows
   #:add-map-rows!
   #:add-mapping-rows!))

(cl:in-package #:cl-data-frame)



;;; Ordered keys provide a mapping from column keys (symbols) to nonnegative
;;; integers.  They are used internally and the corresponding interface is
;;; NOT EXPORTED.

(defstruct (ordered-keys (:copier nil))
  "Representation of ordered keys.

TABLE maps keys to indexes, starting from zero."
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
  (mapcar #'car
          (sort (hash-table-alist (ordered-keys-table ordered-keys))
                #'<=
                :key #'cdr)))

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
  (let+ (((&structure ordered-keys- table) ordered-keys)
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
  (let+ (((&structure ordered-keys- table) ordered-keys))
    (make-ordered-keys :table (copy-hash-table table))))

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



(defclass data-frame ()
  ((ordered-keys
    :initarg :ordered-keys
    :type ordered-keys)
   (columns
    :initarg :columns
    :type vector
    :reader columns-vector)))

(defun make-data-frame (ordered-keys columns)
  "Create a data frame from ORDERED-KEYS and COLUMNS (can be any kind of
sequence).  FOR INTERNAL USE.  Always creates a copy of COLUMNS in order to
ensure that it is an adjustable array with a fill pointer."
  (let ((n-columns (length columns)))
    (make-instance 'data-frame
                   :ordered-keys ordered-keys
                   :columns (make-array n-columns
                                        :adjustable t
                                        :fill-pointer n-columns
                                        :initial-contents columns))))

(defun data-frame-length (data-frame)
  "Length of DATA-FRAME (number of rows)."
  (column-length (aref (columns-vector data-frame) 0)))

(defun column (data-frame key)
  "Return the column of DATA-FRAME corresponding to KEYS."
  (let+ (((&slots-r/o ordered-keys columns) data-frame))
    (aref columns (key-index ordered-keys key))))

(defun data-frame-keys (data-frame)
  "List of keys."
  (key-list (slot-value data-frame 'ordered-keys)))

(defun data-frame-alist (data-frame)
  "Key-column pairs as an alist."
  (map 'list #'cons (data-frame-keys data-frame) (columns-vector data-frame)))

(defun data-frame-plist (data-frame)
  "Key-column pairs as a plist."
  (alist-plist (data-frame-alist data-frame)))

(defun alist-data-frame (key-and-column-alist)
  "Create a data from an alist of KEYs and COLUMNs.  Columns are checked for matching length."
  (assert key-and-column-alist () "Can't create an empty data frame.")
  (let* ((columns (mapcar #'cdr key-and-column-alist))
         (length (column-length (car columns))))
    (assert (every (lambda (column)
                     (= length (column-length column)))
                   (cdr columns)))
    (make-data-frame (ordered-keys (mapcar #'car key-and-column-alist))
                     columns)))

(defun plist-data-frame (key-and-column-plist)
  "Create a data from a plist of KEYs and COLUMNs.  Columns are checked for matching length."
  (assert key-and-column-plist () "Can't create an empty data frame.")
  (alist-data-frame (plist-alist key-and-column-plist)))

(defun data-frame (&rest plist-or-alist)
  "Create a data from a plist or alist of KEYs and COLUMNs.  Columns are checked for matching length.

If the first argument is a CONS, the rest are assumed to be conses (hence an alist), otherwise the arguments are considered a PLIST."
  (if (consp (car plist-or-alist))
      (alist-data-frame plist-or-alist)
      (plist-data-frame plist-or-alist)))

(defmethod print-object ((data-frame data-frame) stream)
  (print-unreadable-object (data-frame stream :type t)
    (let ((alist (data-frame-alist data-frame)))
      (format stream "~d x ~d" (length alist) (data-frame-length data-frame))
      (loop for (key . column) in alist
            do (format stream "~&  ~A  ~A"
                       key (column-summary column))))))

(defun copy-data-frame (data-frame)
  "Create a copy of a data frame."
  (let+ (((&slots-r/o ordered-keys columns) data-frame))
    (make-data-frame (copy-ordered-keys ordered-keys)
                     columns))) ; NOTE: make-data-frame copies columns

(defun add-column! (data-frame key column)
  "Modify DATA-FRAME by adding COLUMN with KEY.  Return DATA-FRAME."
  (let+ (((&slots ordered-keys columns) data-frame))
    (assert (= (column-length column) (data-frame-length data-frame)))
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
  "Return a new data frame with keys and columns added.  Does not modify
DATA-FRAME."
  (aprog1 (copy-data-frame data-frame)
    (apply #'add-columns! it key-and-column-plist)))



;;; implementation of SLICE for DATA-FRAME

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



;;; mapping rows and adding columns

(defun map-rows (data-frame keys function &key (element-type t))
  "Map rows using FUNCTION, on the columns corresponding to KEYS.  Return the
result with the given ELEMENT-TYPE."
  (let ((columns (map 'list (curry #'column data-frame) keys))
        (length (data-frame-length data-frame)))
    (aprog1 (make-array length :element-type element-type)
      (dotimes (index length)
        (setf (aref it index)
              (apply function
                     (mapcar (lambda (column)
                               (ref column index))
                             columns)))))))

(defun select-rows (data-frame keys predicate)
  "Return a bit-vector containing the result of calling PREDICATE on rows of
the columns corresponding to KEYS (0 for NIL, 1 otherwise)."
  (map-rows data-frame keys (compose (lambda (flag)
                                       (if flag 1 0))
                                     predicate)
            :element-type 'bit-vector))



;;; macros

(defun process-bindings (bindings)
  "Return forms for variables and keys as two values, for use in macros.

BINDINGS is a list of (VARIABLE &optional KEY) forms, where VARIABLE is a
symbol and KEY is evaluated.  When KEY is not given, it is VARIABLE converted
to a keyword.

NOT EXPORTED."
  (let ((alist (mapcar (lambda+ ((variable
                                  &optional (key (make-keyword variable))))
                         (check-type variable symbol)
                         (cons variable key))
                       bindings)))
    (values (mapcar #'car alist)
            `(list ,@(mapcar #'cdr alist)))))

(defun keys-and-lambda-from-bindings (bindings body)
  "Process bindings and return a form that can be spliced into the place of
KEYS and FUNCTION (using BODY) in functions that map rows.  NOT EXPORTED."
  (unless body
    (warn "Empty function body."))
  (let+ (((&values variables keys) (process-bindings bindings)))
    `(,keys (lambda ,variables ,@body))))

(defmacro mapping-rows ((data-frame bindings &key (element-type t))
                         &body body)
  "Map rows of DATA-FRAME and return the resulting column (with the given
ELEMENT-TYPE).  See MAP-ROWS.

BINDINGS is a list of (VARIABLE KEY) forms, binding the values in each row to
the VARIABLEs for the columns designated by KEYs."
  `(map-rows ,data-frame
             ,@(keys-and-lambda-from-bindings bindings body)
             :element-type ,element-type))

(defmacro selecting-rows ((data-frame bindings) &body body)
  "Map rows using predicate and return the resulting bit vector (see
SELECT-ROWS).

BINDINGS is a list of (VARIABLE KEY) forms, binding the values in each row to
the VARIABLEs for the columns designated by KEYs."
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
     (defmacro ,macro ((data-frame key bindings &key (element-type t))
                       &body body)
       ,(format nil
"Map rows of DATA-FRAME and add the resulting column (with the given
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
    (add-map-rows add-columns)
    add-mapping-rows)

(define-map-add-function-and-macro
    "Modify (and also return) DATA-FRAME."
    (add-map-rows! add-column!)
    add-mapping-rows!)


