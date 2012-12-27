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

(defsuite data-frame-creation (data-frame))

(deffixture data-frame-creation (@body)
  (let ((v #(1 2 3))
        (b #*0110)
        (s #(a b c d)))
    @body))

(deftest data-frame-creation (data-frame-creation)
  (let ((df (data-frame :vector v :symbols s)))
    (assert-equalp `(:vector ,v :symbols ,s)
        (columns-plist df))
    (assert-equalp '(:vector :symbols)
        (column-names df))))
