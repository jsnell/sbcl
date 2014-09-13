(in-package "SB!EVAL-MC")

(sb!kernel::!defstruct-with-alternate-metaclass
 minimally-compiled-function
 :slot-names (name lambda-list documentation source-location source-path)
 :boa-constructor %make-minimally-compiled-function
 :superclass-name function
 :metaclass-name sb!kernel:static-classoid
 :metaclass-constructor sb!kernel:make-static-classoid
 :dd-type sb!kernel:funcallable-structure
 :runtime-type-checks-p nil)

#-sb-xc-host
(progn
  (defun make-minimally-compiled-function
      (name lambda-list documentation source-location source-path function)
    (let ((mincfun (%make-minimally-compiled-function
                    name lambda-list documentation source-location source-path)))
      (setf (sb!kernel:funcallable-instance-fun mincfun) function)
      mincfun))

  (defun minimally-compiled-function-p (function)
    (typep function 'minimally-compiled-function))

  (sb!int:def!method print-object ((obj minimally-compiled-function) stream)
    (print-unreadable-object (obj stream
                              :identity (not (minimally-compiled-function-name obj)))
      (format stream "~A ~A" '#:minimally-compiled-function
              (minimally-compiled-function-name obj)))))

;; An internal variable set by the implementation of LET to track
;; special variables to bind.
(defvar *dyn-vars*)

;; An internal variable set by the implementation of LET to track
;; values to bind to special variables.
(defvar *dyn-vals*)

(declaim (type (member :execute :execute-tlf :compile-time-too :not-compile-time)
               *mode*))
(defvar *mode* :execute
  "The processing mode for COMPILE-FORM.")

;; The current lexical context.  Only valid in the dynamic context of
;; a COMPILE-FORM call.
(defvar *context*)

(declaim (type integer *more* *arg-count*))

;; The &MORE argument context of the currently executing
;; MINIMALLY-COMPILED-FUNCTION.
(defvar *more*)

;; The argument count of the currently executing
;; MINIMALLY-COMPILED-FUNCTION.
(defvar *arg-count*)

;; A box containing a pointer to the environment of the currently
;; executing MINIMALLY-COMPILED-FUNCTION.
;;
;; This is used for debugging purposes only.
(defvar *env-box*)
