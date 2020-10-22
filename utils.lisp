(defpackage :deepl/utils
  (:use :cl)
  (:import-from :closer-mop)
  (:export
   :string-list-p
   :map-bound-slots))
(in-package :deepl/utils)

#+sbcl(sb-ext:lock-package *package*)

(defun string-list-p (value)
  (and (listp value)
       (do ((rest value (cdr rest)))
           ((atom rest) (null rest))
         (unless (stringp (car rest))
           (return nil)))))

(defun map-bound-slots (function instance)
  (loop :for slot :in (closer-mop:class-slots (class-of instance))
        :for slot-name := (closer-mop:slot-definition-name slot)
        :when (slot-boundp instance slot-name)
        :do (funcall function slot (slot-value instance slot-name))))
