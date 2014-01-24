;;;; package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:policy-cond
  (:use #:cl)
  (:export
   #:policy-if
   #:policy-cond)
  (:export
   #:with-expectations))

(defpackage #:policy
  (:use #:cl)
  (:shadow #:if)
  (:export #:if))
