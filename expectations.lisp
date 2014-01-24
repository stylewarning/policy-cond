;;;; expectations.lisp
;;;;
;;;; Author: Robert Smith

(in-package #:policy-cond)

(defmacro with-expectations (policy (&rest expectations) &body body)
  "Execute BODY with expectations laid out by the clauses EXPECTATIONS when the policy expression POLICY holds true. When POLICY does not hold true, then EXPECTATIONS will be explicitly checked at runtime.

EXPECTATIONS should be lists of one of the following forms.

    Type Expectation: (TYPE <type> <vars-or-exprs>...)

      Assert that the variables and expressions <vars-or-exprs> should
      have the type <type>. If the POLICY is met, then declarations
      will be made for the variables only.

    Assertion Expectation: (ASSERTION <assertion>)

      Assert that the assertion <assertion> should be true. If the
      POLICY is met, then the assertion will be elided at runtime.


"
  (labels ((parse-safe-expectation (e)
             (assert (listp e) (e) "Expected an expectation clause. Got ~S" e)
             (ecase (intern (symbol-name (car e)) :keyword)
               ((:type) (let ((type (second e))
                              (vars (cddr e)))
                          (mapcar (lambda (var)
                                    `(check-type ,var ,type))
                                  vars)) )
               ((:assertion) (list `(assert ,(second e))))))
           (parse-speedy-expectation (e)
             (assert (listp e) (e) "Expected an expectation clause. Got ~S" e)
             (ecase (intern (symbol-name (car e)) :keyword)
               ((:type) (let ((type (second e))
                              (vars (remove-if-not #'symbolp (cddr e))))
                          (list `(type ,type ,@vars))))
               ((:assertion) nil))))
    `(policy-if
      ,policy
      ,(if (null expectations)
           `(progn ,@body)
           `(locally (declare ,@(mapcan #'parse-speedy-expectation expectations))
              ,@body))
      (progn 
        ,@(mapcan #'parse-safe-expectation expectations)
        ,@body))))
