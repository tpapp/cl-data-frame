;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:cl-data-frame-tests
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:clunit
   #:let-plus
   #:cl-slice
   #:cl-data-frame))

(cl:in-package #:cl-data-frame-tests)

(defsuite data-frame ())

(defun run (&optional interactive?)
  (run-suite 'data-frame :use-debugger interactive?))

(defsuite data-frame-basics (data-frame))

(deffixture data-frame-basics (@body)
  (let ((v #(1 2 3))
        (b #*0110)
        (s #(a b c d)))
    @body))

(deftest data-frame-creation (data-frame-basics)
  (let ((df (data-frame :vector v :symbols s)))
    (assert-equalp '(:vector :symbols)
        (column-names df))
    (assert-equalp (vector v s)
        (columns-vector df))
    (assert-equalp `(:vector ,v :symbols ,s)
        (columns-plist df))
    (assert-equalp `((:vector . ,v) (:symbols . ,s))
        (columns-alist df))))

(deftest data-frame-slice (data-frame-basics)
  (let ((df (data-frame :vector v :symbols s)))
    (assert-equalp `(:vector ,v)
        (columns-plist (slice df t #(:vector))))
    (assert-equalp `(:vector ,(slice v b))
        (columns-plist (slice df b #(0))))
    (assert-equalp (slice v b)
        (slice df b :vector))))
