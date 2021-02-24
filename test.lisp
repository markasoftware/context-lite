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

(test defmethod-call-next-method ;;FUCKFUCKFUCK. Can we wrap the next-methods in compute-discriminating-function?
  "call-next-method, with and without arguments"
  (fmakunbound 'do-the-thing)
  (defvar *indicator* nil)
  (defmethod* do-the-thing (c) ()
    c)
  (defmethod* do-the-thing (c) ((*indicator* number))
    (cons c (call-next-method (* c 3))))
  (defmethod* do-the-thing (c) ((*indicator* integer))
    (cons c (call-next-method (+ c 7))))

  (is (= 51 (let ((*indicator* 0)) (do-the-thing 10)))))

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
  "&optional, etc in the normal lambda list should not break special variables")

(test setf-function
  "Test that all our functions work on setf functions"
  (fmakunbound '(setf do-the-thing))
  (let (set-me)
    (defgeneric* (setf do-the-thing) (new-value))
    (defmethod* (setf do-the-thing) (new-value) ()
      42)
    ;; ensure-generic*-function? It's not really worth it, it's such a light wrapper.
    (is (= 42 (setf (do-the-thing) 2)))))

(test defmethod-documentation)

(test defmethod-implicit-block
  "Check the defmethod body is wrapped in a correctly named block"
  (fmakunbound 'do-the-thing)
  (defmethod* do-the-thing () ()
    (return-from do-the-thing t)
    nil)

  (is (do-the-thing)))

(test defmethod-vanilla
  "Make sure the non-star DEFMETHOD still works.")

(test defmethod-redefine)

(test defmethod-qualifiers)

(run! context-lite)
