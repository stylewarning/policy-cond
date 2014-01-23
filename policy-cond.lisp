;;;; policy-cond.lisp
;;;; Author: Robert Smith

(in-package #:policy-cond)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-policy (env)
    "Get the optimize policy information for the environment ENV."
    #+sbcl
    (sb-cltl2:declaration-information 'optimize env)
    
    #+lispworks
    (hcl:declaration-information 'optimize env)
    
    #+cmucl
    (ext:declaration-information 'optimize env)

    #+ccl
    (ccl:declaration-information 'optimize env)
    
    #-(or sbcl lispworks cmucl ccl)
    (error "Declaration information is unavailable for this implementation.")))

(defmacro policy (expr env)
  (let ((policy (get-policy env) ))
    `(let ,policy
       (declare (ignorable ,@(mapcar #'car policy)))
       ,expr)))

(defmacro policy-if (expr then else &environment env)
  "If the policy expression EXPR is true, then expand into THEN,
otherwise into ELSE. The policy expression is as described in
POLICY-COND."
  (if (eval `(policy ,expr ,env))
      then
      else))
  
(defmacro policy-cond (&body cases)
  "Like COND, except each clause predicate is a policy expression. A
policy expression is a boolean expression using optimize declaration
qualities such as SPEED, SAFETY, DEBUG, COMPILATION-SPEED, etc. as if
they're lexically bound to their actual value.

The result of POLICY-COND will be the first clause whose policy
expression is satisfied. This is computed at compile time based off
the current compiler policy."
  (if (null cases)
      (error "No policy expression was satisfied.")
      `(policy-if ,(caar cases)
                  (progn ,@(cdar cases))
                  (policy-cond ,@(cdr cases)))))

