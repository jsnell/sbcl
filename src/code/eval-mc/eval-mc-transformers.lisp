;;;; A minimally compiling EVAL (source transforms).

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

(defvar *go-tag-index-mapping* nil)
(defvar *go-tag-catch-tag-mapping* nil)

(defmacro %go (tag)
  (declare (ignore tag))
  (error "GO outside of TAGBODY"))

(defmacro %parsed-tagbody (&body body)
  ;; We wrap each TAGBODY in a CATCH form.  Since TAGBODYs can be
  ;; nested, we track a mapping from TAGBODY labels to their
  ;; corresponding catch-tags.
  ;;
  ;; A GO form is translated into a THROW to the appropriate
  ;; catch-tag.  The value thrown is the index of the tagbody block we
  ;; want to jump to.
  ;;
  ;; The implementation of %TAGBODY handles the actual looping and
  ;; jumping.
  (let* ((catch-tag (gensym "TAGBODY-CATCH-TAG"))
         (labels-and-bodies (parse-tagbody-tags-and-bodies body))
         (tagbody-labels (mapcar 'first labels-and-bodies))
         (tagbody-blocks (mapcar 'rest labels-and-bodies)))
    `(compiler-let ((*go-tag-catch-tag-mapping*
                      (append ',(loop for label in tagbody-labels
                                      collect `(,label . ,catch-tag))
                              *go-tag-catch-tag-mapping*))
                    (*go-tag-index-mapping*
                      (append ',(loop for label in tagbody-labels
                                      for i from 0
                                      collect `(,label . ,i))
                              *go-tag-index-mapping*)))
       (macrolet ((%go (tag)
                    `(throw ',(cdr (assoc tag *go-tag-catch-tag-mapping* :test 'eql))
                       ,(cdr (assoc tag *go-tag-index-mapping* :test 'eql)))))
         (%tagbody (,catch-tag)
           ,@tagbody-blocks)))))