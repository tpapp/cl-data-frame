;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:cl-data-frame
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:array-operations
   #:let-plus
   #:cl-slice
   #:cl-slice-dev)
  (:export
   ;; ordered keys
   #:ordered-keys
   #:invalid-key
   #:duplicate-key
   #:key-index
   #:key-list
   ;; data frame
   #:data-frame
   #:columns-vector
   #:columns-alist
   #:columns-plist
   #:column-keys
   #:column))

(cl:in-package #:cl-data-frame)



(defstruct ordered-keys
  (keys nil :type list :read-only t)
  (table nil :type hash-table :read-only t))

(define-condition invalid-key (error)
  ;; FIXME should contain more info
  ())

(define-condition duplicate-key (error)
  ;; FIXME should contain more info
  ())

(defun ordered-keys (keys)
  (let ((table (make-hash-table :test #'eq :size (length keys)))
        (index 0))
    (map nil (lambda (key)
               (check-type key symbol)
               (let+ (((&values &ign present?) (gethash key table)))
                 (when present?
                   (error 'duplicate-key)))
               (setf (gethash key table) index)
               (incf index))
         keys)
    (make-ordered-keys :keys keys
                       :table table)))

(defun key-index (ordered-keys key)
  "Return the index for KEY."
  (let+ (((&values index present?)
          (gethash key (ordered-keys-table ordered-keys))))
    (unless present?
      (error 'invalid-key))
    index))

(defun key-list (ordered-keys)
  "List of all keys."
  (ordered-keys-keys ordered-keys))

(defmethod axis-dimension ((axis ordered-keys))
  (hash-table-count (ordered-keys-table axis)))

(defmethod canonical-representation ((axis ordered-keys) (slice symbol))
  (key-index axis slice))

(defmethod slice ((ordered-keys ordered-keys) &rest slices)
  (let+ (((slice) slices))
    (ordered-keys
     (slice (key-list ordered-keys)
            (canonical-representation ordered-keys slice)))))

(defmethod print-object ((ordered-keys ordered-keys) stream)
  (print-unreadable-object (ordered-keys stream :type t)
    (format stream "~{~a~^, ~}" (key-list ordered-keys))))



(defclass data-frame ()
  ((ordered-keys :initarg :ordered-keys)
   (columns :initarg :columns :type vector :reader columns-vector)))

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

(defun data-frame (&rest keys-and-columns-plist)
  (let* ((alist (plist-alist keys-and-columns-plist)))
    (make-instance 'data-frame
                   :ordered-keys (ordered-keys (mapcar #'car alist))
                   :columns (coerce (mapcar #'cdr alist) 'vector))))

(defmethod slice ((data-frame data-frame) &rest slices)
  (let+ (((row-slice &optional (column-slice t)) slices)
         ((&slots-r/o ordered-keys columns) data-frame)
         (column-slice (canonical-representation ordered-keys column-slice))
         (columns (slice columns column-slice))
         ((&flet slice-column (column)
            (slice column row-slice))))
    (if (singleton-representation? column-slice)
        (slice-column columns)
        (make-instance 'data-frame
                       :ordered-keys (slice ordered-keys column-slice)
                       :columns (map 'vector #'slice-column columns)))))

;;; TODO: (setf slice)
