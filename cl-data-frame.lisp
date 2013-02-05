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

(defmethod print-object ((summary bit-vector-summary) stream)
  (let+ (((&structure-r/o bit-vector-summary- length count) summary))
    (format stream "bits, ones: ~A (~A%)"
            count
            (round (/ count length) 1/100))))

(defstruct numeric-vector-summary
  "Summary of a numeric vector."
  (length 0 :type array-index :read-only t)
  (real-count 0 :type array-index :read-only t)
  (min 0 :type real :read-only t)
  (q25 0 :type real :read-only t)
  (q50 0 :type real :read-only t)
  (q75 0 :type real :read-only t)
  (max 0 :type real :read-only t))

(defun ensure-not-ratio (real)
  "When REAL is a RATIO, convert it to a float, otherwise return as is.  Used for printing."
  (if (typep real 'ratio)
      (float real 1.0)
      real))

(defmethod print-object ((summary numeric-vector-summary) stream)
  (let+ (((&structure-r/o numeric-vector-summary- real-count min q25 q50 q75 max)
          summary))
    (format stream "~A reals, min:~A  q25:~A  q50:~A  q75:~A  max:~A"
            real-count min (ensure-not-ratio q25)
            (ensure-not-ratio q50) (ensure-not-ratio q75) max)))

(defun non-numeric-column-summary (column)
  (declare (ignore column))
  "column is not numeric FIXME need to implement this summary")

(defgeneric column-summary (column)
  (:documentation "Return an object that summarizes COLUMN of a DATA-FRAME.  Primarily intended for printing, not analysis, returned values should print nicely.")
  (:method ((column bit-vector))
    (make-bit-vector-summary :length (length column) :count (count 1 column)))
  (:method ((column vector))
    (let+ ((length (length column))
           (elements (loop for elt across column
                           when (realp elt)
                           collect elt)))
      (if (<= (length elements) (/ length 2))
          (non-numeric-column-summary column)
          (let+ ((#(min q25 q50 q75 max)
                   (clnu:quantiles elements #(0 1/4 1/2 3/4 1))))
            (make-numeric-vector-summary :length (length column)
                                         :real-count (length elements)
                                         :min min
                                         :q25 q25
                                         :q50 q50
                                         :q75 q75
                                         :max max))))))


(cl:defpackage #:cl-data-frame
  (:nicknames #:data-frame #:dframe)
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:let-plus
   #:cl-data-frame.column
   #:cl-slice
   #:cl-slice-dev)
  (:export
   ;; error messages for ordered keys
   #:duplicate-key
   #:key-not-found
   ;; generic - both data-vector and data-frame
   #:columns
   #:map-columns
   #:column
   #:keys
   #:copy
   #:as-alist
   #:as-plist
   #:add-columns
   #:add-column!
   #:add-columns!
   ;; data-vector
   #:data-vector
   #:make-dv
   #:alist-dv
   #:plist-dv
   #:dv
   ;; data-frame
   #:data-frame
   #:nrow
   #:make-df
   #:alist-df
   #:plist-df
   #:df
   #:matrix-df
   ;; transformations for data-frames
   #:map-rows
   #:do-rows
   #:mask-rows
   #:count-rows
   #:map-df
   #:mapping-rows
   #:mapping-df
   #:masking-rows))

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

(defun keys-count (ordered-keys)
  "Number of keys."
  (hash-table-count (ordered-keys-table ordered-keys)))

(defun keys-vector (ordered-keys)
  "Vector of all keys."
  (map 'vector #'car
       (sort (hash-table-alist (ordered-keys-table ordered-keys))
             #'<=
             :key #'cdr)))

(defun key-index (ordered-keys key)
  "Return the index for KEY."
  (let+ (((&values index present?)
          (gethash key (ordered-keys-table ordered-keys))))
    (unless present?
      (error 'key-not-found :key key :keys (keys-vector ordered-keys)))
    index))

(defmethod print-object ((ordered-keys ordered-keys) stream)
  (print-unreadable-object (ordered-keys stream :type t)
    (format stream "~{~a~^, ~}" (keys-vector ordered-keys))))

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
  (if (slice-reserved-symbol? slice)
      (call-next-method)
      (key-index axis slice)))

(defmethod slice ((ordered-keys ordered-keys) &rest slices)
  (let+ (((slice) slices))
    (ordered-keys
     (slice (keys-vector ordered-keys)
            (canonical-representation ordered-keys slice)))))


;;; generic implementation -- the class is not exported, only the functionality

(defclass data ()
  ((ordered-keys
    :initarg :ordered-keys
    :type ordered-keys)
   (columns
    :initarg :columns
    :type vector))
  (:documentation "This class is used for implementing both data-vector and data-matrix, and represents and ordered collection of key-column pairs.  Columns are not assumed to have any specific attributes.  This class is not exported."))

(defun make-data (class keys columns)
  "Create a DATA object from KEYS and COLUMNS.  FOR INTERNAL USE.  Always creates a copy of COLUMNS in order to ensure that it is an adjustable array with a fill pointer.  KEYS are converted to ORDERED-KEYS if necessary."
  (let ((n-columns (length columns))
        (ordered-keys (atypecase keys
                        (ordered-keys it)
                        (t (ordered-keys it)))))
    (assert (= n-columns (keys-count ordered-keys)))
    (assert (subtypep class 'data))
    (make-instance class
                   :ordered-keys ordered-keys
                   :columns (make-array n-columns
                                        :adjustable t
                                        :fill-pointer n-columns
                                        :initial-contents columns))))

(defgeneric check-column-compatibility (data column)
  (:documentation "Check if COLUMN is compatible with DATA.")
  (:method ((data data) column)))

(defun ensure-arguments-alist (rest)
  "Recognizes the following and converts them to an alist:

  plist
  alist
  (plist)
  (alist)
  (data-frame)"
  (let+ (((&flet error% (&optional (list rest))
            (error "Could not interpret ~A as a plist or alist." list)))
         ((&flet ensure-alist (list)
            (typecase (car list)
              (cons rest)
              (symbol (plist-alist rest))
              (t (error% list))))))
    (if (cdr rest)
        (ensure-alist rest)
        (let ((first (car rest)))
          (typecase first
            (data (as-alist first))
            (cons (if (consp (cdr first))
                      (ensure-alist first)
                      rest))            ; first element of an alist
            (t (error%)))))))

(defun alist-data (class alist)
  "Create an object of CLASS (subclass of DATA) from ALIST which contains key-column pairs."
  (assert alist () "Can't create an empty data frame.")
  (make-data class (mapcar #'car alist) (mapcar #'cdr alist)))

(defun plist-data (class plist)
  "Create an object of CLASS (subclass of DATA) from PLIST which contains keys and columns, interleaved."
  (alist-data class (plist-alist plist)))

(defun guess-alist? (plist-or-alist)
  "Test if the argument is an ALIST by checking its first element.  Used for deciding which creation function to call."
  (consp (car plist-or-alist)))

(defun keys (data)
  "List of keys."
  (check-type data data)
  (copy-seq (keys-vector (slot-value data 'ordered-keys))))

(defun as-alist (data)
  "Key-column pairs as an alist."
  (check-type data data)
  (map 'list #'cons (keys data) (columns data)))

(defun as-plist (data)
  "Key-column pairs as a plist."
  (check-type data data)
  (alist-plist (as-alist data)))

(defun copy (data &key (key #'identity))
  "Copy data frame or vector.  Keys are copied (and thus can be modified), columns or elements are copyied using KEY, making the default give a shallow copy."
  (check-type data data)
  (let+ (((&slots-r/o ordered-keys columns) data))
    (make-data (class-of data)
               (copy-ordered-keys ordered-keys)
               (map 'vector key columns))))

(defun column (data key)
  "Return column corresponding to key."
  (check-type data data)
  (let+ (((&slots-r/o ordered-keys columns) data))
    (aref columns (key-index ordered-keys key))))

(defun (setf column) (column data key)
  "Set column corresponding to key."
  (check-column-compatibility data column)
  (let+ (((&slots-r/o ordered-keys columns) data))
    (setf (aref columns (key-index ordered-keys key)) column)))

(defun columns (data &optional (slice t))
  "Return the columns as a vector, or a slice if given (keys are resolved)."
  (check-type data data)
  (let+ (((&slots-r/o ordered-keys columns) data))
    (slice columns (canonical-representation ordered-keys slice))))

(defun map-columns (data function &optional (result-class (class-of data)))
  "Map columns of DATA-FRAME or DATA-VECTOR using FUNCTION.  The result is a new DATA-FRAME with the same keys."
  (make-data result-class (keys data) (map 'vector function (columns data))))

(defun add-column! (data key column)
  "Modify DATA (a data-frame or data-vector) by adding COLUMN with KEY.  Return DATA."
  (check-column-compatibility data column)
  (let+ (((&slots ordered-keys columns) data))
    (add-key! ordered-keys key)
    (vector-push-extend column columns))
  data)

(defun add-columns! (data &rest keys-and-columns)
  "Modify DATA (a data-frame or data-vector) by adding columns with keys (see README about accepted argument formats)."
  (mapc (lambda+ ((key . column))
          (add-column! data key column))
        (ensure-arguments-alist keys-and-columns))
  data)

(defun add-columns (data &rest keys-and-columns)
  "Return a new data-frame or data-vector with keys and columns added.  Does not modify DATA (see README about accepted argument formats)."
  (aprog1 (copy data)
    (apply #'add-columns! it keys-and-columns)))

(defmacro define-data-subclass (class abbreviation)
  (check-type class symbol)
  (check-type abbreviation symbol)
  (let+ (((&flet fname (prefix)
            (symbolicate prefix '#:- abbreviation)))
         (alist-fn (fname '#:alist))
         (plist-fn (fname '#:plist)))
    `(progn
       (defclass ,class (data)
         ())
       (defun ,(fname '#:make) (keys columns)
         (make-data ',class keys columns))
       (defun ,alist-fn (alist)
         (alist-data ',class alist))
       (defun ,plist-fn (plist)
         (plist-data ',class plist))
       (defun ,abbreviation (&rest plist-or-alist)
         (if (guess-alist? plist-or-alist)
             (,alist-fn plist-or-alist)
             (,plist-fn plist-or-alist))))))

(define-data-subclass data-vector dv)

(defmethod print-object ((data-vector data-vector) stream)
  (print-unreadable-object (data-vector stream :type t)
    (let ((alist (as-alist data-vector)))
      (format stream "~d" (length alist))
      (loop for (key . column) in alist
            do (format stream "~&  ~A  ~A"
                       key column)))))

(defmethod slice ((data-vector data-vector) &rest slices)
  (let+ (((column-slice) slices)
         ((&slots-r/o ordered-keys columns) data-vector)
         (column-slice (canonical-representation ordered-keys column-slice)))
    (if (singleton-representation? column-slice)
        (aref columns column-slice)
        (make-dv (slice ordered-keys column-slice)
                 (slice columns column-slice)))))

(define-data-subclass data-frame df)

(defmethod initialize-instance :after ((data-frame data-frame) &rest initargs)
  (declare (ignore initargs))
  (let+ (((first . rest) (coerce (columns data-frame) 'list))
         (length (column-length first)))
    (assert (every (lambda (column)
                     (= length (column-length column)))
                   rest)
            () "Columns don't have the same length.")))

(defun nrow (data-frame)
  "Number of rows in DATA-FRAME."
  (check-type data-frame data-frame)
  (column-length (aref (columns data-frame) 0)))

(defmethod check-column-compatibility ((data data-frame) column)
  (assert (= (column-length column) (nrow data))))

(defmethod print-object ((data-frame data-frame) stream)
  (print-unreadable-object (data-frame stream :type t)
    (let ((alist (as-alist data-frame)))
      (format stream "~d x ~d" (length alist) (nrow data-frame))
      (loop for (key . column) in alist
            do (format stream "~&  ~A  ~A"
                       key (column-summary column))))))

(defun matrix-df (keys matrix)
  "Convert a matrix to a data-frame with the given keys."
  (let+ ((columns (aops:split (clnu:transpose matrix) 1)))
    (assert (length= columns keys))
    (alist-df (map 'list #'cons keys columns))))

(defun df-matrix (data-frame)
  "Return contents of DATA-FRAME as a matrix."
  (clnu:transpose (aops:combine (columns data-frame))))

;;; implementation of SLICE for DATA-FRAME

(defmethod slice ((data-frame data-frame) &rest slices)
  (let+ (((row-slice &optional (column-slice t)) slices)
         ((&slots-r/o ordered-keys columns) data-frame)
         (row-slice (canonical-representation (nrow data-frame) row-slice))
         (column-slice (canonical-representation ordered-keys column-slice))
         (columns (slice columns column-slice))
         ((&flet slice-column (column)
            (slice column row-slice))))
    (if (singleton-representation? column-slice)
        (slice-column columns)
        (let ((keys (slice ordered-keys column-slice))
              (columns (map 'vector #'slice-column columns)))
          (if (singleton-representation? row-slice)
              (make-dv keys columns)
              (make-df keys columns))))))

;;; TODO: (setf slice)

;;; mapping rows and adding columns

(defun map-rows (data-frame keys function &key (element-type t))
  "Map rows using FUNCTION, on the columns corresponding to KEYS.  Return the result with the given ELEMENT-TYPE."
  (let ((columns (map 'list (curry #'column data-frame) (ensure-list keys)))
        (nrow (nrow data-frame)))
    (aprog1 (make-array nrow :element-type element-type)
      (dotimes (index nrow)
        (setf (aref it index)
              (apply function
                     (mapcar (lambda (column)
                               (ref column index))
                             columns)))))))

(defun do-rows (data-frame keys function)
  "Traverse rows from first to last, calling FUNCTION on the columns corresponding to KEYS.  Return no values."
  (let ((columns (map 'list (curry #'column data-frame) (ensure-list keys)))
        (nrow (nrow data-frame)))
    (dotimes (index nrow (values))
      (apply function
             (mapcar (lambda (column)
                       (ref column index))
                     columns)))))

(defun map-df (data-frame keys function result-keys)
  "Map DATA-FRAME to another one by rows.  Function is called on the columns corresponding to KEYS, and should return a sequence with the same length as RESULT-KEYS, which give the keys of the resulting data frame.  RESULT-KETS should be either symbols, or of the format (symbol &optional (element-type t))."
  (let* ((columns (map 'list (curry #'column data-frame) keys))
         (nrow (nrow data-frame))
         (result-keys-and-element-types
           (mapcar (lambda (key-and-element-type)
                     (let+ (((key &optional (element-type t))
                             (ensure-list key-and-element-type)))
                       (cons key element-type)))
                   result-keys))
         (result-columns (map 'vector
                              (lambda (key-and-element-type)
                                (make-array nrow
                                            :element-type (cdr key-and-element-type)))
                              result-keys-and-element-types)))
    (dotimes (index nrow)
      (let ((result-row (apply function
                               (mapcar (lambda (column)
                                         (ref column index))
                                       columns))))
        (assert (length= result-row result-columns))
        (map nil (lambda (result-column result-element)
                   (setf (aref result-column index) result-element))
             result-columns result-row)))
    (make-df (mapcar #'car result-keys-and-element-types) result-columns)))

(defun mask-rows (data-frame keys predicate)
  "Return a bit-vector containing the result of calling PREDICATE on rows of the columns corresponding to KEYS (0 for NIL, 1 otherwise)."
  (map-rows data-frame keys (compose (lambda (flag)
                                       (if flag 1 0))
                                     predicate)
            :element-type 'bit-vector))

(defun count-rows (data-frame keys predicate)
  "Count the number of rows for which PREDICATE called on the columns corresponding to KEYS returns non-NIL."
  (let ((columns (map 'list (curry #'column data-frame) (ensure-list keys))))
    (loop for index below (nrow data-frame)
          count (apply predicate
                       (mapcar (lambda (column)
                                 (ref column index))
                               columns)))))


;;; macros

(defun process-bindings (bindings)
  "Return forms for variables and keys as two values, for use in macros.

BINDINGS is a list of (VARIABLE &optional KEY) forms, where VARIABLE is a symbol and KEY is evaluated.  When KEY is not given, it is VARIABLE converted to a keyword.

NOT EXPORTED."
  (let ((alist (mapcar (lambda (binding)
                         (let+ (((variable &optional (key (make-keyword variable)))
                                 (ensure-list binding)))
                           (check-type variable symbol)
                           (cons variable key)))
                       bindings)))
    (values (mapcar #'car alist)
            `(list ,@(mapcar #'cdr alist)))))

(defun keys-and-lambda-from-bindings (bindings body)
  "Process bindings and return a form that can be spliced into the place of KEYS and FUNCTION (using BODY) in functions that map rows.  NOT EXPORTED."
  (unless body
    (warn "Empty function body."))
  (let+ (((&values variables keys) (process-bindings bindings)))
    `(,keys (lambda ,variables ,@body))))

(defmacro mapping-rows ((data-frame bindings &key (element-type t))
                         &body body)
  "Map rows of DATA-FRAME and return the resulting column (with the given ELEMENT-TYPE).  See MAP-ROWS.

BINDINGS is a list of (VARIABLE KEY) forms, binding the values in each row to the VARIABLEs for the columns designated by KEYs."
  `(map-rows ,data-frame
             ,@(keys-and-lambda-from-bindings bindings body)
             :element-type ,element-type))

(defmacro mapping-df ((data-frame bindings result-keys)
                      &body body)
  "Map rows of DATA-FRAME to another data-frame.  See MAP-DF.

BINDINGS is a list of (VARIABLE KEY) forms, binding the values in each row to the VARIABLEs for the columns designated by KEYs."
  `(map-df ,data-frame
             ,@(keys-and-lambda-from-bindings bindings body)
             ,result-keys))

(defmacro masking-rows ((data-frame bindings) &body body)
  "Map rows using predicate and return the resulting bit vector (see MASK-ROWS).

BINDINGS is a list of (VARIABLE KEY) forms, binding the values in each row to the VARIABLEs for the columns designated by KEYs."
  `(mask-rows ,data-frame
              ,@(keys-and-lambda-from-bindings bindings body)))
