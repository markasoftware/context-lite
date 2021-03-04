#| MIT License

Copyright (c) 2021 Mark Polyakov

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

|#

(defpackage #:context-lite/test
  (:use #:cl #:context-lite #:fiveam))

(in-package #:context-lite/test)

(defvar *indicator* nil)
(defvar *a* nil)
(defvar *b* nil)

(in-suite* context-lite)

(test defmethod-no-special-vars
  "Create a generic* function using defmethod*, do not specialize on any special variables, and
      then run that method."
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing (a b c) ()
    (* a (+ b c)))
  (is (= 5 (do-the-thing 1 2 3))))

(test defmethod-multiple-no-special-vars
  "Test eql and class specializers normally"
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing (a b c) ()
    (* a (+ b c)))
  (defmethod* do-the-thing ((a (eql 42)) b c) ()
    (declare (ignore b c))
    999)
  (defmethod* do-the-thing ((a string) b c) ()
    (declare (ignore b c))
    1001)

  (is (= 5 (do-the-thing 1 2 3)))
  (is (= 999 (do-the-thing 42 9 8)))
  (is (= 1001 (do-the-thing "42" 9 8))))

(test defmethod-only-special-vars
  "Specializing on only one special var"
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing () ()
    nil)
  (defmethod* do-the-thing () ((*indicator* (eql :active)))
    t)

  (is (null (do-the-thing)))
  (is (eql t (let ((*indicator* :active)) (do-the-thing)))))

(test defmethod-mixed
  "Specializing on both arguments and special variables"
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing (a) ()
    0)
  (defmethod* do-the-thing (a) ((*indicator* (eql :active)))
    1)
  (defmethod* do-the-thing ((a string)) ((*indicator* number))
    2)

  (is (= 0 (do-the-thing "hello")))
  (is (= 0 (let ((*indicator* 42)) (do-the-thing 999))))
  (is (= 1 (let ((*indicator* :active)) (do-the-thing 5))))
  (is (= 2 (let ((*indicator* 42)) (do-the-thing "heyhi")))))

(test defmethod-special-precedence
  "Check whether precedence between special variable is, without intervention, equal to the
      order they were added."
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing () ((*a* (eql 42)))
    :a)
  (defmethod* do-the-thing () ((*b* (eql 42)))
    :b)

  (is (eq :a (let ((*a* 42) (*b* 99)) (do-the-thing))))
  (is (eq :b (let ((*a* 99) (*b* 42)) (do-the-thing))))
  (is (eq :a (let ((*a* 42) (*b* 42)) (do-the-thing))))

  (defmethod* do-the-thing () ((*a* (eql 42)) (*b* (eql 42)))
    :c)

  (is (eq :c (let ((*a* 42) (*b* 42)) (do-the-thing)))))

(test defmethod-call-next-method
  "call-next-method, with and without arguments"
  (fmakunbound 'do-the-thing)
  (defvar *indicator* nil)
  (defmethod* do-the-thing (c) ()
    c)
  (defmethod* do-the-thing (c) ((*indicator* number))
    (call-next-method))
  (defmethod* do-the-thing (c) ((*indicator* integer))
    (call-next-method (+ c 7)))

  (is (= 17 (let ((*indicator* 0)) (do-the-thing 10)))))

(test defgeneric-special-precedence
  "Precedence overrides specified in initial defgeneric"
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing () ((*a* (eql 42)))
    0)
  (defmethod* do-the-thing () ((*b* (eql 42)))
    1)

  (is (= 0 (let ((*a* 42) (*b* 42)) (do-the-thing))))

  (defgeneric* do-the-thing () (:special-variable-precedence-order *b* *a*))

  (is (= 1 (let ((*a* 42) (*b* 42)) (do-the-thing)))))

(test advanced-normal-lambda-list
  "&optional, etc in the normal lambda list should not break special variables"
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing (a &optional b) ((*indicator* number))
    (declare (ignore b))
    172)
  (defmethod* do-the-thing ((a (eql 42)) &optional b) ((*indicator* (eql :active)))
    (declare (ignore b))
    999)

  (is (= 172 (let ((*indicator* 5)) (do-the-thing 42 "hi"))))
  (is (= 172 (let ((*indicator* 5)) (do-the-thing 19))))
  (is (= 999 (let ((*indicator* :active)) (do-the-thing 42 1)))))

(test setf-function
  "Test that all our functions work on setf functions"
  (fmakunbound '(setf do-the-thing))
  (defgeneric* (setf do-the-thing) (new-value))
  (defmethod* (setf do-the-thing) (new-value) ()
    42)
  ;; ensure-generic*-function? It's not really worth it, it's such a light wrapper.
  (is (= 42 (setf (do-the-thing) 2))))

(test defmethod-implicit-block
  "Check the defmethod body is wrapped in a correctly named block"
  (fmakunbound 'do-the-thing)
  ;; find-method doesn't really work, so i don't know why I bother checking this...
  (let ((my-method
          (defmethod* do-the-thing () ()
            "Do things."
            (declare (optimize (debug 3)))
            (return-from do-the-thing t)
            nil)))
    (is (string= "Do things." (documentation my-method t))))

  (is (do-the-thing)))

(test defmethod-redefine
  "Check that redefining a method works"
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing (k) ((*a* (eql 5)))
    "hi")
  (defmethod* do-the-thing (k) ((*a* (eql 5)))
    "bye")
  ;; ensure that there's only one
  (is (= 1 (length (context-lite::generic*-inner-methods #'do-the-thing))))
  ;; and ensure it's the right one
  (is (string= "bye" (let ((*a* 5)) (do-the-thing nil))))

  (defmethod* do-the-thing ((k number)) ((*a* (eql 5))))
  (is (= 2 (length (context-lite::generic*-inner-methods #'do-the-thing))))

  (defmethod* do-the-thing ((k string)) ((*a* string) (*b* number))
    99)
  (defmethod* do-the-thing ((k string)) ((*b* number) (*a* string))
    100)
  (is (= 3 (length (context-lite::generic*-inner-methods #'do-the-thing))))
  (is (= 100 (let ((*b* 9) (*a* "blah")) (do-the-thing "str"))))
  (defmethod* do-the-thing :after ((k string)) ((*b* number) (*a* string))
    100)
  (is (= 4 (length (context-lite::generic*-inner-methods #'do-the-thing))))
  )

(test defmethod-qualifiers
  "Check that qualifiers work as normal."
  (fmakunbound 'do-the-thing)
  (let (i)
    (defmethod* do-the-thing :before () ((*a* number))
      (incf i))
    (defmethod* do-the-thing :before () ()
      (incf i 3))
    (defmethod* do-the-thing () ((*a* (eql 5)))
      (setf i (* i 2)))
    (defmethod* do-the-thing () ())
    (defmethod* do-the-thing :after () ((*a* integer))
      (setf i (* i 3)))

    (setf i 5)
    (do-the-thing)
    (is (= 8 i))

    (setf i 5)
    (let ((*a* 5)) (do-the-thing))
    (is (= 54 i))

    (setf i 5)
    (let ((*a* 1.5)) (do-the-thing))
    (is (= 9 i))))

(test defmethod-invalid-lambda-list
  "Check that, when we provide a normal lambda list that's incompatible with the previous normal
  lambda list, that an error is signaled and the existing method is not broken."
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing (arg1) ()
    5)
  (signals error
    (defmethod* do-the-thing (arg1 arg2) ((*a* (eql 9)))
      6))
  ;; re-add all the wrapper methods, in case it changes anything
  (is (= 5 (do-the-thing 99))))

(test defmethod-invalid-lambda-list-2
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing ((arg1 (eql 0)) arg2) ()
    1)
  (defmethod* do-the-thing (arg1 (arg2 (eql 0))) ()
    2)
  (is (= 1 (do-the-thing 0 0)))
  (signals error
    (defgeneric* do-the-thing (arg1 arg2 arg3)
      (:argument-precedence-order arg2 arg1)))
  ;; even if our code is broken and saves the wrong precedence order, it won't be observable unless
  ;; we define a new method that triggers re-adding all the wrapper methods.
  (defmethod* do-the-thing (arg1 arg2) ((*a* number)))
  (is (= 1 (do-the-thing 0 0))))

(test defmethod-no-bind
  "Check that the special variables don't get a local binding during method body"
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing () ((*a* (eql 0)))
    (incf *a*))
  (let ((*a* 0))
    (do-the-thing)
    (is (= 1 *a*))))

(test different-method-combination
  "Check that it still works when the method combination is not default."
  (fmakunbound 'do-the-thing)
  (defgeneric* do-the-thing (arg)
    (:method-combination append))
  (defmethod* do-the-thing append (arg) ()
    (declare (ignore arg))
    '(base))
  (defmethod* do-the-thing append (arg) ((*a* (eql :baljeep)))
    (declare (ignore arg))
    '(baljeep))
  (defmethod* do-the-thing append ((arg integer)) ()
    '(integer))

  (is (equal '(base) (do-the-thing nil)))
  (is (equal '(baljeep base) (let ((*a* :baljeep)) (do-the-thing nil))))
  (is (equal '(integer baljeep base) (let ((*a* :baljeep)) (do-the-thing 5)))))

(run! 'context-lite)

;; (defun crappy-benchmark (num-vars num-methods num-trials)
;;   (assert (plusp num-vars))
;;   (assert (plusp num-methods))
;;   (assert (plusp num-trials))
;;   (let ((method-name (gensym))
;;         (special-vars (loop repeat num-vars
;;                             for var = (gensym)
;;                             do (defvar var nil)
;;                             collect var)))
;;     (loop repeat num-methods
;;           do (eval `(defmethod ,method-name)))

    
;;     (loop repeat num-trials)))
