;;;; package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:policy-cond
  (:use #:cl)
  (:export
   #:declaration-information
   #:policy-if
   #:policy-cond
   #:with-policy)
  (:export
   #:with-expectations))

(defpackage #:policy
  (:use #:cl)
  (:shadow #:if)
  (:export #:if))
