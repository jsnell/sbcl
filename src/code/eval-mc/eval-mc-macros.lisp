;;;; A minimally compiling EVAL (utility macros)

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, this file was
;;;; written from scratch after the fork from CMU CL.
;;;;
;;;; The software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "SB!EVAL-MC")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +stack-max+ 8))

(defmacro with-dynamic-extent-environment ((var debug-record parent size) &body body)
  "Bind VAR to a newly stack-allocated environment of size SIZE with parent PARENT."
  (let ((data% (gensym))
        (size% (gensym)))
    `(let* ((,size% ,size)
            (,data% (make-array (list ,size%)))
            (,var (%make-environment ,debug-record ,parent ,data%)))
       (declare (type (mod #.(1+ +stack-max+)) ,size%)
                ;; we must not allocate environment objects on the
                ;; stack unless we can be sure that all child
                ;; environments will also be allocated on the stack,
                ;; but we can't really know that.
                (dynamic-extent ,var)
                (dynamic-extent ,data%))
       ,@body)))

(defmacro with-indefinite-extent-environment ((var debug-record parent size) &body body)
  "Bind VAR to a newly heap-allocated environment of size SIZE with parent PARENT."
  (let ((data% (gensym))
        (size% (gensym)))
    `(let* ((,size% ,size)
            (,data% (make-array (list ,size%)))
            (,var (%make-environment ,debug-record ,parent ,data%)))
       (declare (fixnum ,size%))
       ,@body)))

(defmacro with-parsed-body ((forms-var specials-var policy-var) exprs &body body)
  "Parse EXPRS as a list of declarations and forms. Execute BODY with FORMS-VAR bound to the list of forms, SPECIALS-VAR bound to the list of variables declared special by the parsed declarations, and POLICY-VAR bound to a list of declared optimization policy settings."
  (let ((decls (gensym)))
    `(multiple-value-bind (,decls ,forms-var) (body-decls&forms ,exprs)
       (let ((,specials-var (reduce #'append (mapcar #'decl-specials ,decls)))
             (,policy-var (reduce #'append (mapcar #'decl-policy ,decls))))
         (declare (ignorable ,policy-var))
         ,@body))))

(defmacro with-context (context &body body)
  `(let ((*context* ,context))
     ,@body))

(defmacro specialize (var value possible-values &body body)
  "Specialize BODY over POSSIBLE-VALUES.

BODY is an implicit progn that is expected to return a form that
references VAR.  For each value in POSSIBLE-VALUES, the form is
instantiated with VAR bound to the value.  At runtime, the form
corresponding to the value that is the same under EQL as the result of
evaluating VALUE will be chosen and executed."
  `(ecase ,value
     ,@(loop for x in (cl:eval possible-values)
             collect
                `((,x) ,(cl:eval `(let ((,var ,x)) ,@body))))))

(defmacro nlet (loop-var bindings &body body)
  "Named LET.  Apply the BINDINGS and execute BODY, with LOOP-VAR bound to a local function that restarts BODY with its arguments as new values for the variables bound in BINDINGS."
  `(labels ((,loop-var ,(mapcar #'first bindings)
              ,@body))
     (,loop-var ,@(mapcar #'second bindings))))

(defmacro dnlet (loop-var bindings &body body)
  "Named LET with dynamic extent.  The same as NLET except that LOOP-VAR is declared DYNAMIC-EXTENT."
  `(labels ((,loop-var ,(mapcar #'first bindings)
              ,@body))
     (declare (dynamic-extent #',loop-var))
     (,loop-var ,@(mapcar #'second bindings))))
