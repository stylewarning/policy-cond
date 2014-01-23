;;;; policy.lisp

(in-package #:policy)

(defmacro if (form)
  "Intended to be used with read macros. FORM should be a policy
expression as described in POLICY-COND:POLICY-COND. For example,

#+#.(policy:if (> speed safety)) EXPRESSION"
  (labels ((replace-keys (form)
             (typecase form
               (keyword
                (or (find-symbol (symbol-name form) :cl)
                    form))
               (cons
                (mapcar #'replace-keys form))
               (t
                form))))
    `(policy-cond:policy-if ,(replace-keys form) '(:and) '(:or))))
