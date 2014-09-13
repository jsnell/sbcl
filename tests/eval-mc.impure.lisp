;;;; various tests of the minimal compiler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

#-sb-eval
(sb-ext:exit :code 104)

(setf sb-ext:*evaluator-mode* :minimally-compile)

(defvar *fun* (lambda ()))

(with-test (:name (:lambda-is-minimally-compiled))
  (assert (typep *fun* 'sb-eval-mc::minimally-compiled-function)))

(with-test (:name (:minimally-compiled-is-compiled))
  (assert (typep *fun* 'compiled-function)))

(with-test (:name (:minimally-compiled-is-compiled-function-p))
  (assert (compiled-function-p *fun*)))
