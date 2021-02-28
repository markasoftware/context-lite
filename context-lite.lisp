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

(defclass generic*-function ()
  ((name
    :accessor generic*-name
    :initarg :name
    :initform (error "generic* functions need a :name"))
   (normal-lambda-list
    :accessor generic*-normal-lambda-list
    :initform nil)
   (normal-argument-precedence-order
    :accessor generic*-normal-argument-precedence-order
    :initform nil)
   (special-variable-precedence-order
    :accessor generic*-special-variable-precedence-order
    :initform nil)
   (inner-methods
    :accessor generic*-inner-methods
    :initform nil)
   (inner-gf
    :accessor generic*-gf)
   (wrapper-method-table
    :accessor generic*-wrapper-method-table
    :initform (make-hash-table)))
  (:metaclass c2mop:funcallable-standard-class))

(defmethod remove-all-wrapper-methods ((gf generic*-function))
  (loop for method in (c2mop:generic-function-methods (generic*-gf gf))
        do (remove-method (generic*-gf gf) method))
  ;; this is the only point where wrapper methods are removed
  (setf (generic*-wrapper-method-table gf) (make-hash-table)))

(defmethod add-all-wrapper-methods ((gf generic*-function))
  (loop for inner-method in (generic*-inner-methods gf)
        do (add-method* gf inner-method)))

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

(defmacro accessor-transaction (accessors instance &body body)
  "Save the value of the accessors before executing body. If an error is signaled during body,
  revert the accessors back to their saved values. Each accessor must be a symbol naming an accessor
  function."
  (let ((instance-var (gensym "INSTANCE"))
        (saved-value-list-var (gensym "SAVED-VALUE-LIST")))
    `(let* ((,instance-var ,instance)
            (,saved-value-list-var (list ,@(loop for accessor in accessors
                                                 do (assert (symbolp accessor))
                                                 collect `(,accessor ,instance-var)))))
       (handler-bind
           ((error (lambda (err)
                     (declare (ignore err))
                     ,@(loop for accessor in accessors
                             nconc `((setf (,accessor ,instance-var) (car ,saved-value-list-var))
                                     (setf ,saved-value-list-var (cdr ,saved-value-list-var)))))))
         (progn ,@body)))))

(defmethod generic*-install-discriminating-function ((gf generic*-function))
  (let ((inner-discriminating-function (c2mop:compute-discriminating-function (generic*-gf gf))))
    ;; TODO: should I use (compile)?
    ;; TODO: is there any reason/way to explicitly list the args instead of using &rest?
    (eval `(c2mop:set-funcallable-instance-function
            ,gf
            (lambda (&rest args)
              (apply ,inner-discriminating-function
                     ;; TODO: specialize on a variable simply being bound and non-nil?
                     ,@(generic*-special-variable-precedence-order gf)
                     args))))))

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

(defmethod ensure-generic*-function-using-class
    ((gf generic*-function) fn-name &rest options
     &key
       (argument-precedence-order nil argument-precedence-order-p)
       (special-variable-precedence-order nil special-variable-precedence-order-p)
       (special-variables nil special-variables-p)
       (lambda-list nil lambda-list-p)
       (generic-function-class 'standard-generic-function)
       (method-class 'standard-method)
     &allow-other-keys)

  (with-accessors ((normal-argument-precedence-order
                    generic*-normal-argument-precedence-order)
                   (special-variable-precedence-order-slot
                    generic*-special-variable-precedence-order)
                   (normal-lambda-list
                    generic*-normal-lambda-list)
                   (fn-name-slot
                    fn-name))
      gf

    ;; use the identity of the special variable list to determine, later, whether or not we need to
    ;; remove and regenerate all the wrapper methods, so that if a special variable list is
    ;; specified but doesn't cause anything to change, nothing changes. (append) is guaranteed to
    ;; change identity so this can't go wrong. This is to avoid n^2 performance with respect to the
    ;; number of methods, when defining a new method. Likely a premature optimization.
    (let ((original-sv-precedence-order special-variable-precedence-order-slot))
      (setf (generic*-name gf) fn-name)
      (when lambda-list-p ;; TODO: if the lambda list is invalid, the error will occur during the
        ;; later ensure-generic-function-using-class call, but the lambda list
        ;; will have already been permanently stored! Same goes for other params.
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
      (apply #'c2mop:ensure-generic-function-using-class
             (generic*-gf gf) (c2mop:generic-function-name (generic*-gf gf))
             :argument-precedence-order (append normal-argument-precedence-order
                                                special-variable-precedence-order-slot)
             :lambda-list (append special-variable-precedence-order-slot
                                  normal-lambda-list)
             :generic-function-class generic-function-class
             :method-class method-class
             (remove-from-plist options
                                :argument-precedence-order
                                :special-variable-precedence-order
                                :special-variables
                                :generic-function-class
                                :lambda-list))
      (unless (eq original-sv-precedence-order special-variable-precedence-order-slot)
        (add-all-wrapper-methods gf))
      gf)

    ;; symbol not bound yet
    (setf (fdefinition fn-name) gf)))

(defun ensure-generic*-function (fn-name &rest initargs)
  ;; TODO: custom class?
  (unless (and (fboundp fn-name) (subtypep (type-of (fdefinition fn-name)) 'generic*-function))
    (setf (fdefinition fn-name) (make-instance 'generic*-function :name fn-name)))
  (apply 'ensure-generic*-function-using-class
         (fdefinition fn-name) fn-name initargs))

(defmethod initialize-instance :after
    ((gf generic*-function) &key name)

  (setf (generic*-gf gf) (c2mop:ensure-generic-function
                          (gensym (string (if (consp name) (cadr name) name)))
                          ;; sbcl loses its shit if you don't specify the lambda list, presumably
                          ;; because defgeneric always provides :lambda-list
                          :lambda-list nil))
  (generic*-install-discriminating-function gf))

;; (defmethod reinitialize-instance ((gf generic*-function) &rest args)
;;   ;; TODO properly
;;   (call-next-method))

#-(or ccl abcl)
(defmethod wrap-method-function ((gf generic*-function) (method method*) num-special-vars)
  (let ((method-function (c2mop:method-function method)))
    (lambda (args nexts)
      (funcall method-function
               (nthcdr num-special-vars args)
               (mapcar (lambda (next) (gethash next (generic*-wrapper-method-table gf))) nexts)))))

;; minor amop violation from abcl: method-functions take a single function where the next-methods
;; argument would normally go, and this function (which they perhaps erroneously call an "effective
;; method") handles calling all of the next methods in turn. The "emfun" takes a list of arguments
;; as its argument.
#+abcl
(defmethod wrap-method-function ((gf generic*-function) (method method*) num-special-vars)
  (let ((method-function (c2mop:method-function method)))
    (compile nil `(lambda (args next-emfun)
                    (funcall ,method-function
                             (nthcdr ,num-special-vars args)
                             (lambda (args)
                               (funcall
                                next-emfun
                                (list*
                                 ,@(generic*-special-variable-precedence-order gf)
                                 args))))))))

;; HACK. CCL's methods get some special info by way of an &method parameter. I'm not sure exactly
;; the nature of this parameter, but it seems like it gets filled out incorrectly on the inner
;; methods, so we can do apply-with-method-context to override the parameter with whatever was
;; passed to the wrapper method.
#+ccl
(defmethod wrap-method-function ((gf generic*-function) (method method*) num-special-vars)
  (let ((method-function (c2mop:method-function method)))
    (lambda (ccl::&method methvar &rest args)
      (ccl::apply-with-method-context methvar method-function (nthcdr num-special-vars args)))))

(defmethod add-method* ((gf generic*-function) (method method*))
  ;; here's where we wrap the method* into a method
  (let ((method-special-vars (method*-special-variables method)))
    ;; first, make sure that the generic function knows about all our special variables
    (ensure-generic*-function-using-class
     gf (generic*-name gf) :special-variables (mapcar #'car method-special-vars))
    (unless (member method (generic*-inner-methods gf))
      (push method (generic*-inner-methods gf)))
    ;; now, wrap the method into a standard-method and add it normally
    (let* ((gf-special-vars (generic*-special-variable-precedence-order gf))
           (wrapper-method
             (make-instance
              'standard-method
              :documentation (documentation method t)
              :qualifiers (method-qualifiers method)
              :function (wrap-method-function gf method (length gf-special-vars))
              :lambda-list (append gf-special-vars (c2mop:method-lambda-list method))
              :specializers
              (append (loop for gf-special-var in gf-special-vars
                            collect (or (cdr (assoc gf-special-var method-special-vars))
                                        (find-class t)))
                      (c2mop:method-specializers method)))))
      #-ecl
      (setf (gethash wrapper-method (generic*-wrapper-method-table gf)) method)
      #+ecl
      (setf (gethash (c2mop:method-function wrapper-method) (generic*-wrapper-method-table gf))
            (c2mop:method-function method))
      (add-method (generic*-gf gf) wrapper-method
                  ;; TODO: look into accessor methods
                  )
      (generic*-install-discriminating-function gf)
      method)))

(defmacro defgeneric* (name lambda-list &rest options)
  ;; strategy: use defgeneric for all teh options supported by defgeneric, then call
  ;; ensure-generic-function to add special options

  ;; TODO: allow custom generic-function-class and method-class
  (let ((ensure-options (list :name (if (consp name) `',(cadr name) `',name)
                              :lambda-list `',lambda-list))
        declarations)
    (loop for option in options
          do (ecase (car option)
               ((:special-variables :special-variable-precedence-order :argument-precedence-order
                                    :documentation :method-combination)
                (push `',(cdr option) ensure-options)
                (push (car option) ensure-options))
               ('declare
                (setf declarations (nconc declarations (cdr option))))))

    (when declarations
      (push `',declarations ensure-options)
      (push :declarations ensure-options))
             
    `(ensure-generic*-function ',name ,@ensure-options)))

(defmacro defmethod* (name &rest args)
  ;; strategy: create a vanilla method on a fresh generic function, so we can reuse most of the
  ;; implementations defmethod logic. Then, create a method* on the generic* object
  (let* ((block-name (if (consp name) (cadr name) name))
         (name-string (string block-name))
         (temp-gf-name (gensym name-string))
         vanilla-args                   ; with special lambda list removed
         special-variables              ; List of (symbol . specializer-form)
         )
    (flet ((specializer-name->specializer-form (name)
             (etypecase name
               (symbol `(find-class ',name))
               (cons
                (assert (eq (car name) 'eql) (name) "Invalid specializer ~a" name)
                (assert (= 2 (length name)) (name) "Invalid specializer ~a" name)
                `(c2mop:intern-eql-specializer ,(cadr name)))))
           #+ccl
           (make-block (body)
             `(flet
                  ((call-next-method (&rest args)
                     (if args
                         (apply #'call-next-method
                                ;; TODO: performance?
                                (append
                                 (mapcar #'symbol-value
                                         (generic*-special-variable-precedence-order actual-gf))
                                 args))
                         (call-next-method))))
                (block ,block-name ,@body)))
           #-ccl
           (make-block (body)
             `(block ,block-name ,@body)))
      
      (loop with state = :pre
            with found-doc
            for (arg . rest) on args
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
                  (cond
                    ((and (stringp arg) rest (not found-doc))
                     (push arg vanilla-args)
                     (setf found-doc t))
                    ((and (consp arg) (eq 'declare (car arg)))
                     (push arg vanilla-args))
                    (t
                     (push (make-block (cons arg rest)) vanilla-args)
                     (return)))))))
    (setf vanilla-args (nreverse vanilla-args))

    ;; we have to create the gf-var before the temp method, even though we update the gf-var right
    ;; afterwards, because the method refers to the gf-var in its call-next-method binding.
    `(let* ((actual-gf (ensure-generic*-function ',name))
            (temp-method (defmethod ,temp-gf-name ,@vanilla-args))
            (actual-gf
              (ensure-generic*-function
               ',name
               :lambda-list (c2mop:generic-function-lambda-list
                             (symbol-function ',temp-gf-name))
               :special-variables '(,@(mapcar #'car special-variables))))
            (actual-method 
              (add-method*
               actual-gf
               (make-instance 'method*
                              :special-variables
                              (list ,@(loop for (var . specializer-form) in special-variables
                                            collect `(cons ',var ,specializer-form)))
                              :qualifiers (method-qualifiers temp-method)
                              :specializers (c2mop:method-specializers temp-method)
                              :lambda-list (c2mop:method-lambda-list temp-method)
                              :documentation (documentation temp-method t)
                              :function (c2mop:method-function temp-method)))))
       ;; CCL doesn't use the documentation initarg, ffs
       (setf (documentation actual-method t)
             (documentation temp-method t))
       actual-method)))

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
