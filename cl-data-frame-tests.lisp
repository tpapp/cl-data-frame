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
  (let ((v #(1 2 3 4))
        (b #*0110)
        (s #(a b c d)))
    @body))

(deftest data-frame-creation (data-frame-basics)
  (let ((df (data-frame :vector v :symbols s)))
    (assert-equalp '(:vector :symbols)
        (data-frame-keys df))
    (assert-equalp (vector v s)
        (columns-vector df))
    (assert-equalp `(:vector ,v :symbols ,s)
        (data-frame-plist df))
    (assert-equalp `((:vector . ,v) (:symbols . ,s))
        (data-frame-alist df))))

(deftest data-frame-slice (data-frame-basics)
  (let ((df (data-frame :vector v :symbols s)))
    (assert-equalp `(:vector ,v)
        (data-frame-plist (slice df t #(:vector))))
    (assert-equalp `(:vector ,(slice v b))
        (data-frame-plist (slice df b #(0))))
    (assert-equalp (slice v b)
        (slice df b :vector))))

(deftest data-frame-map (data-frame-basics)
  (let ((df (data-frame :a #(2 3 5)
                        :b #(7 11 13)))
        (product #(14 33 65)))
    (assert-equalp product
        (map-rows df '(:a :b) #'*))
    (assert-equalp product
        (with-map-rows (df)
                       ((a :a)
                        (b :b))
          (* a b)))))
