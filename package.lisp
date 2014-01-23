;;;; package.lisp
;;;;
;;;; Author: Robert Smith

(defpackage #:policy-cond
  (:use #:cl)
  (:export
   #:policy-if
   #:policy-cond))

(defpackage #:policy
  (:use #:cl)
  (:shadow #:if)
  (:export #:if))
