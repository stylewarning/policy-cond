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
  (labels ((keywordify (s)
             (intern (symbol-name s) :keyword))
           
           (validate-expectation (e)
             (assert (listp e) (e) "Expected an expectation clause. Got ~S" e)
             (case (keywordify (car e))
               ((:type)
                (assert (cdr e) () "Invalid type expectation: ~S" e)
                (assert (cddr e) () "Empty variable/expression list in type expectation: ~S"
                        e))
               
               ((:assertion)
                (assert (and (cdr e)
                             (null (cddr e)))
                        ()
                        "Invalid assertion expectation: ~S"
                        e))
               
               (otherwise (warn "Ignoring unrecognized expectation: ~S" e))))
           
           (parse-safe-expectation (e)
             (case (keywordify (car e))
               ((:type) (let ((type (second e))
                              (vars (cddr e)))
                          (mapcar (lambda (var)
                                    `(check-type ,var ,type))
                                  vars)) )
               ((:assertion) (list `(assert ,(second e))))))
           
           (parse-speedy-expectation (e)
             (case (keywordify (car e))
               ((:type) (let ((type (second e))
                              (vars (remove-if-not #'symbolp (cddr e))))
                          (list `(type ,type ,@vars))))
               ((:assertion) nil))))
    (mapc #'validate-expectation expectations)
    `(policy-if
      ,policy
      ,(if (null expectations)
           `(progn ,@body)
           `(locally (declare ,@(mapcan #'parse-speedy-expectation expectations))
              ,@body))
      (progn 
        ,@(mapcan #'parse-safe-expectation expectations)
        ,@body))))
