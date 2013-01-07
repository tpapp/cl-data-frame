;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

(cl:defpackage #:cl-data-frame-tests
  (:use
   #:cl
   #:alexandria
   #:anaphora
   #:clunit
   #:let-plus
   #:cl-slice
   #:cl-data-frame)
  (:export
   #:run))

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
  (let* ((plist `(:vector ,v :symbols ,s))
         (df (apply #'data-frame plist))
         (df-plist (plist-data-frame plist))
         (df-alist (alist-data-frame (plist-alist plist))))
    (assert-equalp '(:vector :symbols)
        (data-frame-keys df))
    (assert-equalp (vector v s)
        (columns-vector df))
    (assert-equalp `(:vector ,v :symbols ,s)
        (data-frame-plist df))
    (assert-equalp `((:vector . ,v) (:symbols . ,s))
        (data-frame-alist df))
    (assert-equalp (data-frame-alist df)
        (data-frame-alist df-plist))
    (assert-equalp (data-frame-alist df)
        (data-frame-alist df-alist))))

(deftest data-frame-slice (data-frame-basics)
  (let ((df (data-frame :vector v :symbols s)))
    (assert-equalp `(:vector ,v)
        (data-frame-plist (slice df t #(:vector))))
    (assert-equalp `(:vector ,(slice v b))
        (data-frame-plist (slice df b #(0))))
    (assert-equalp (slice v b)
        (slice df b :vector))))

(deftest data-frame-map (data-frame-basics)
  (let+ ((df (data-frame :a #(2 3 5)
                         :b #(7 11 13)))
         (product #(14 33 65))
         ((&flet predicate (a b) (<= 30 (* a b)))))
    (assert-equalp product
        (map-rows df '(:a :b) #'*))
    (assert-equalp product
        (mapping-rows (df ((a :a)
                           (b :b)))
          (* a b)))
    (assert-equalp #*011
        (select-rows df '(:a :b) #'predicate))
    (assert-equalp #*011
        (selecting-rows (df ((a :a)
                             (b :b)))
          (predicate a b)))))

(deftest print-object (data-frame-basics)
  (let ((df (data-frame :a v :b b)))
    (assert-true (with-output-to-string (stream)
                   (print-object df stream)))))

;;;

(defsuite data-frame-add (data-frame))

(deffixture data-frame-add (@body)
  (let* ((plist1 '(:a #(1 2 3)))
         (plist2 '(:b #(4 5 6)))
         (plist12 (append plist1 plist2)))
    @body))

(defmacro test-add (add-function plist1 plist2 append?)
  "Macro for generating the following test:

  1. create a data frame using plist1,

  2. add plist2 using add-function to get a second data frame,

  3. test that the first data frame is uncorrupted if append? is nil, or
     equivalent the concatenated plist otherwise,

  4. test that the second data frame is equivalent to the concatenated plist.

This is a pretty comprehensive test of the add-column family of functions,
destructive or non-destructive."
  (with-unique-names (df df2 plist12)
    (once-only (plist1 plist2)
      `(let* ((,df (apply #'data-frame ,plist1))
              (,df2 (apply ,add-function ,df ,plist2))
              (,plist12 (append ,plist1 ,plist2)))
         (assert-equal (if ,append?
                           ,plist12
                           ,plist1)
             (data-frame-plist ,df))
         (assert-equal ,plist12
             (data-frame-plist ,df2))))))

(deftest add-column (data-frame-add)
    (test-add #'add-columns plist1 plist2 nil)
    (test-add #'add-column! plist1 plist2 t)
    (test-add #'add-columns! plist1 plist2 t))

(deftest add-map (data-frame-add)
  (let* ((plist3 '(:c #(4 10 18)))
         (plist123 (append plist12 plist3)))
    ;; non-destructive
    (let* ((df (apply #'data-frame plist12))
           (df2 (add-map-rows df '(:a :b) #'* :c))
           (df3 (add-mapping-rows (df :c
                                   ((a :a)
                                    (b :b)))
                  (* a b))))
      (assert-equalp plist12
          (data-frame-plist df))
      (assert-equalp plist123
          (data-frame-plist df2))
      (assert-equalp plist123
          (data-frame-plist df3)))
    ;; destructive, function
    (let* ((df (apply #'data-frame plist12))
           (df2 (add-map-rows! df '(:a :b) #'* :c)))
      (assert-equalp plist123
          (data-frame-plist df))
      (assert-equalp plist123
          (data-frame-plist df2)))
    ;; destructive, macro
    (let* ((df (apply #'data-frame plist12))
           (df2 (add-mapping-rows! (df :c
                                    ((a :a)
                                     (b :b)))
                  (* a b))))
      (assert-equalp plist123
          (data-frame-plist df))
      (assert-equalp plist123
          (data-frame-plist df2)))))

(deftest empty-body-warnings (data-frame-add)
  (assert-condition warning
      (macroexpand '(mapping-rows (nil nil))))
  (assert-condition warning
      (macroexpand '(add-mapping-rows (nil nil nil))))
  (assert-condition warning
      (macroexpand '(add-mapping-rows! (nil nil nil)))))
