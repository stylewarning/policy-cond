;;;; policy-cond.lisp
;;;;
;;;; Copyright (c) 2014 Robert Smith

(in-package #:policy-cond)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun declaration-information (symbol &optional env)
    "Get the declaration information for the environment ENV."
    #+sbcl
    (sb-cltl2:declaration-information symbol env)

    #+lispworks
    (hcl:declaration-information symbol env)

    #+cmucl
    (ext:declaration-information symbol env)

    #+ccl
    (ccl:declaration-information symbol env)

    #+allegro
    (system:declaration-information symbol env)

    #-(or sbcl lispworks cmucl ccl allegro)
    (cl-environments.cltl2:declaration-information symbol env)))

(defmacro policy (expr env)
  (let ((policy (declaration-information 'optimize env)))
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

(defmacro with-policy (policy &body body &environment env)
  "Execute the body BODY with the global optimize policy set to
POLICY. Once BODY has finished executing, restore the compiler policy
to its original state.

For local declarations, use LOCALLY."
  (let ((saved-policy (declaration-information 'optimize env)))
    `(unwind-protect (progn
                       (proclaim '(optimize ,@policy))
                       ,@body)
       (proclaim '(optimize ,@saved-policy)))))
