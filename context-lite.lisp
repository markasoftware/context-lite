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

(defpackage #:context-lite
  (:nicknames #:clite)
  (:use #:cl)
  (:export #:defgeneric*
           #:defmethod*
           #:ensure-generic*-function))

(in-package #:context-lite)

(define-condition special-lambda-list-error (error) ()
  (:documentation "Invalid special lambda list syntax."))

(defclass generic*-function (c2mop:standard-generic-function)
  ((normal-lambda-list
    :accessor generic*-normal-lambda-list
    :initarg :normal-lambda-list
    :initform nil)
   (normal-argument-precedence-order
    :accessor generic*-normal-argument-precedence-order
    :initarg :normal-argument-precedence-order)
   (special-variable-precedence-order
    :accessor generic*-special-variable-precedence-order
    :initarg :special-variable-precedence-order
    :initform nil)
   (inner-methods
    :accessor generic*-inner-methods
    :initform nil))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod remove-all-wrapper-methods ((gf generic*-function))
  (loop for method in (c2mop:generic-function-methods gf)
        do (remove-method gf method)))

(defmethod add-all-wrapper-methods ((gf generic*-function))
  (loop for inner-method in (generic*-inner-methods gf)
        do (add-method gf inner-method)))

(defmethod generic*-inner-precedence-order ((gf generic*-function))
  (append (generic*-normal-argument-precedence-order gf)
          (generic*-special-variable-precedence-order gf)))

(defclass method* (c2mop:standard-method)
  ((special-variables
    :accessor method*-special-variables
    :initarg :special-variables
    :initform nil
    :documentation "The special lambda list, each element (*special-variable-name* . specializer),
    where specializer is either a class metaobject or an eql-specializer (just like a normal method
    specializer)")))

(defmethod c2mop:compute-discriminating-function ((gf generic*-function))
  (let ((inner-discriminating-function (call-next-method)))
    ;; TODO: should I use (compile)?
    ;; TODO: is there any reason/way to explicitly list the args instead of using &rest?
    (compile nil `(lambda (&rest args)
                    (apply ,inner-discriminating-function
                           ;; TODO: specialize on a variable simply being bound and non-nil?
                           ,@(generic*-special-variable-precedence-order gf)
                           args)))))

(defun combine-special-variable-precedence-ordered (old-order new-order)
  (cond
    ((null old-order) new-order)
    ((null new-order) old-order)
    ((eq (car old-order) (car new-order))
     (cons (car old-order)
           (combine-special-variable-precedence-ordered (cdr old-order) (cdr new-order))))
    ((member (car old-order) new-order)
     (combine-special-variable-precedence-ordered (cdr old-order) new-order))
    (t
     (cons (car old-order)
           (combine-special-variable-precedence-ordered (cdr old-order) new-order)))))

(defun combine-special-variable-precedence-unordered (old-order new-vars)
  ;; maintain identity if nothing changed, so we can speed things up in ensure-
  (let ((really-new-vars (remove-if (lambda (new-var) (member new-var old-order)) new-vars)))
    (if really-new-vars
        (append old-order really-new-vars)
        old-order)))

(defun lambda-list-required-arguments (lambda-list)
  "Given a full lambda list, with specializers, return a list of the required arguments."
  (loop for param-name in (c2mop:extract-lambda-list lambda-list)
        until (member param-name '(&optional &key &rest &aux))
        collect param-name))

(defmethod c2mop:ensure-generic-function-using-class
    ((gf generic*-function) fn-name &rest options
     &key
       (argument-precedence-order nil argument-precedence-order-p)
       (special-variable-precedence-order nil special-variable-precedence-order-p)
       (special-variables nil special-variables-p)
       (lambda-list nil lambda-list-p)
     &allow-other-keys)

  (with-accessors ((normal-argument-precedence-order
                    generic*-normal-argument-precedence-order)
                   (special-variable-precedence-order-slot
                    generic*-special-variable-precedence-order)
                   (normal-lambda-list
                    generic*-normal-lambda-list))
      gf

    ;; use the identity of the special variable list to determine, later, whether or not we need to
    ;; remove and regenerate all the wrapper methods, so that if a special variable list is
    ;; specified but doesn't cause anything to change, nothing changes. (append) is guaranteed to
    ;; change identity so this can't go wrong. This is to avoid n^2 performance with respect to the
    ;; number of methods, when defining a new method. Likely a premature optimization.
    (let ((original-sv-precedence-order special-variable-precedence-order-slot))
      (when lambda-list-p
        (setf normal-lambda-list lambda-list)
        (setf normal-argument-precedence-order (lambda-list-required-arguments lambda-list)))
      (when argument-precedence-order-p
        (assert (= (length argument-precedence-order)
                   (length (lambda-list-required-arguments normal-lambda-list)))
                () "argument-precedence-order must have the same length as the normal lambda list")
        (setf normal-argument-precedence-order argument-precedence-order))
      (when special-variables-p
        (setf special-variable-precedence-order-slot
              (combine-special-variable-precedence-unordered
               special-variable-precedence-order-slot
               special-variables)))
      (when special-variable-precedence-order-p
        (setf special-variable-precedence-order-slot
              (combine-special-variable-precedence-ordered
               special-variable-precedence-order-slot
               special-variable-precedence-order)))
      (unless (eq original-sv-precedence-order special-variable-precedence-order-slot)
        (remove-all-wrapper-methods gf))
      (apply #'call-next-method gf fn-name
             :argument-precedence-order (append normal-argument-precedence-order
                                                special-variable-precedence-order-slot)
             :lambda-list (append special-variable-precedence-order-slot
                                  normal-lambda-list)
             :generic-function-class (or (getf options :generic-function-class) 'generic*-function)
             :method-class (or (getf options :method-class) 'method*)
             (remove-from-plist options
                                :argument-precedence-order
                                :special-variable-precedence-order
                                :special-variables
                                :lambda-list))
      (unless (eq original-sv-precedence-order special-variable-precedence-order-slot)
        (add-all-wrapper-methods gf))
      gf)))

(defmethod initialize-instance ((gf generic*-function) &rest initargs
                                &key argument-precedence-order special-variable-precedence-order
                                  lambda-list
                                &allow-other-keys)

  (unless argument-precedence-order
    (setf argument-precedence-order (lambda-list-required-arguments lambda-list)))
  (apply #'call-next-method gf
         :normal-argument-precedence-order argument-precedence-order
         ;; TODO: default argument precedence order
         :argument-precedence-order (append argument-precedence-order
                                            special-variable-precedence-order)
         :normal-lambda-list lambda-list
         :lambda-list (append special-variable-precedence-order lambda-list)
         (remove-from-plist initargs
                            :argument-precedence-order
                            :special-variable-precedence-order
                            :lambda-list)))

;; (defmethod reinitialize-instance ((gf generic*-function) &rest args)
;;   ;; TODO properly
;;   (call-next-method))

(defmethod add-method ((gf generic*-function) (method method*))
  ;; here's where we wrap the method* into a method
  (let ((method-special-vars (method*-special-variables method))
        (method-function (c2mop:method-function method)))
    ;; first, make sure that the generic function knows about all our special variables
    (c2mop:ensure-generic-function-using-class
     gf (c2mop:generic-function-name gf) :special-variables (mapcar #'car method-special-vars))
    (unless (member method (generic*-inner-methods gf))
      (push method (generic*-inner-methods gf)))
    ;; now, wrap the method into a standard-method and add it normally
    (let ((gf-special-vars (generic*-special-variable-precedence-order gf)))
      (call-next-method
       gf
       (make-instance
        'standard-method
        :documentation (documentation method t)
        :qualifiers (method-qualifiers method)
        :function
        (lambda (args next-methods)
          (funcall method-function
                   (nthcdr (length gf-special-vars) args)
                   ;; TODO: get the inner next-methods instead
                   next-methods))
        :lambda-list (append gf-special-vars
                             (c2mop:method-lambda-list method))
        :specializers (append (loop for gf-special-var in gf-special-vars
                                    collect (or (cdr (assoc gf-special-var method-special-vars))
                                                (find-class t)))
                              (c2mop:method-specializers method))
        ;; TODO: look into accessor methods
        )))))

(defmacro defgeneric* (name lambda-list &rest options)
  ;; strategy: use defgeneric for all teh options supported by defgeneric, then call
  ;; ensure-generic-function to add special options

  ;; TODO: allow custom generic-function-class and method-class
  (let* ((normal-options (list '(:generic-function-class generic*-function)
                               '(:method-class method*)))
         (special-options (list :generic-function-class ''generic*-function
                                :method-class ''method*)))

    (loop for option in options
          do (break)
          do (ecase (car option)
               ((:argument-precedence-order 'declare :documentation :method-combination
                 :method)               ; TODO: disallow?
                (push option normal-options))
               ((:special-variables :special-variable-precedence-order)
                (push `',(cdr option)
                      special-options)
                (push (car option) special-options))))
             
    `(progn
       (defgeneric ,name ,lambda-list ,@normal-options)
       (ensure-generic-function ',name ,@special-options))))

(defmacro defmethod* (name &rest args)
  ;; strategy: create a vanilla method on a fresh generic function, so we can reuse most of the
  ;; implementations defmethod logic. Then, create a method* on the actual generic function 
  (flet ((specializer-name->specializer-form (name)
           (etypecase name
             (symbol `(find-class ',name))
             (cons
              (assert (eq (car name) 'eql) (name) "Invalid specializer ~a" name)
              (assert (= 2 (length name)) (name) "Invalid specializer ~a" name)
              `(c2mop:intern-eql-specializer ,(cadr name))))))
    (let* ((name-string (if (consp name) (string (car name)) (string name))) ; for setf fns
           (temp-gf-name (gensym (concatenate 'string "TEMP-" name-string)))
           (temp-method-var (gensym "temp-method"))
           vanilla-args                 ; with special lambda list removed
           special-variables            ; List of (symbol . specializer-form)
           (gf-var (gensym (concatenate 'string "GF-" name-string))))

      (loop with state = :pre
            for arg in args
            do (ecase state
                 (:pre
                  (push arg vanilla-args)
                  ;; when we reach the lambda list, the next one is the special variable list
                  (when (listp arg)
                    (setf state :special)))
                 (:special
                  (setf special-variables
                        (loop for var in (c2mop:extract-lambda-list arg)
                              for specializer-name in (c2mop:extract-specializer-names arg)
                              for specializer-form
                                = (specializer-name->specializer-form specializer-name)
                              collect (cons var specializer-form)))
                  (setf state :post))
                 (:post
                  (push arg vanilla-args))))
      (setf vanilla-args (nreverse vanilla-args))

      ;; TODO: correct block name
      `(let* ((,temp-method-var (defmethod ,temp-gf-name ,@vanilla-args))
              (,gf-var (ensure-generic-function
                        ',name
                        :lambda-list (c2mop:generic-function-lambda-list
                                      (symbol-function ',temp-gf-name))
                        :special-variables '(,@(mapcar #'car special-variables))
                        :generic-function-class 'generic*-function
                        :method-class 'method*)))
         (add-method ,gf-var
                     (make-instance (c2mop:generic-function-method-class ,gf-var)
                                    :special-variables
                                    (list ,@(loop for (var . specializer-form) in special-variables
                                                  collect `(cons ',var ,specializer-form)))
                                    :qualifiers (method-qualifiers ,temp-method-var)
                                    :specializers (c2mop:method-specializers ,temp-method-var)
                                    :lambda-list (c2mop:method-lambda-list ,temp-method-var)
                                    :documentation (documentation ,temp-method-var t)
                                    :function (c2mop:method-function ,temp-method-var)))))))

;; help out SLIME and prevents compiler warnings
(defmethod c2mop:generic-function-lambda-list ((gf generic*-function))
  (generic*-normal-lambda-list gf))

;; copied from Alexandria
(defun remove-from-plist (plist &rest keys)
  "Returns a propery-list with same keys and values as PLIST, except that keys
in the list designated by KEYS and values corresponding to them are removed.
The returned property-list may share structure with the PLIST, but PLIST is
not destructively modified. Keys are compared using EQ."
  (declare (optimize (speed 3)))
  (loop for (key . rest) on plist by #'cddr
        do (assert rest () "Expected a proper plist, got ~S" plist)
        unless (member key keys :test #'eq)
        collect key and collect (first rest)))
