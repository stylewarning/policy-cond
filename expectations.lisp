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

    Return Type Expectation: (RETURNS [<type>*])

      Assert that the result of a form obeys a certain type. Multiple
      types indicate multiple values are returned. If the POLICY is
      met, then the assertion will be elided at runtime.

    Assertion Expectation: (ASSERTION <assertion>)

      Assert that the assertion <assertion> should be true. If the
      POLICY is met, then the assertion will be elided at runtime.

    Conditional Expectation: (OR-ELSE <predicate> <resulting action>)

     Check that the predicate <predicate> is true, or else perform
     <resulting action>. If the POLICY is met, elide the check and
     action. This clause is principally used for having special
     conditions get raised.
"
  (let ((preamble-forms     nil)
        (local-declarations nil)
        (return-types       :not-provided))
    (labels ((keywordify (s)
               (intern (symbol-name s) :keyword))
             
             (validate-expectation (e)
               (assert (listp e) (e) "Expected an expectation clause. Got ~S" e)
               (case (keywordify (car e))
                 ((:type)
                  (assert (cdr e) () "Invalid type expectation: ~S" e)
                  (assert (cddr e) () "Empty variable/expression list in type expectation: ~S"
                          e))
                 
                 ((:returns) nil)
                 
                 ((:assertion)
                  (assert (and (cdr e)
                               (null (cddr e)))
                          ()
                          "Invalid assertion expectation: ~S"
                          e))
                 
                 ((:or-else)
                  (assert (= 2 (length (cdr e)))
                          ()
                          "Invalid or-else expectation. Expecting a predicate and a result, got: ~S"
                          e))
                 
                 (otherwise (warn "Ignoring unrecognized expectation: ~S" e))))
             
             (parse-safe-expectation (e)
               (case (keywordify (car e))
                 ((:type) (let ((type (second e))
                                (vars (cddr e)))
                            (dolist (var vars)
                              (push `(check-type ,var ,type) preamble-forms))))
                 ((:returns) (setq return-types (cdr e)))
                 ((:assertion) (push `(assert ,(second e)) preamble-forms))
                 ((:or-else) (push `(unless ,(second e)
                                      ,(third e))
                                   preamble-forms))))
             
             (parse-speedy-expectation (e)
               (case (keywordify (car e))
                 ((:type) (let ((type (second e))
                                (vars (remove-if-not #'symbolp (cddr e))))
                            (push `(type ,type ,@vars) local-declarations)))
                 ((:returns) nil)   ; This will already have been parsed.
                 ((:assertion) nil)
                 ((:or-else) nil))))

      ;; Validate the expectations.
      (mapc #'validate-expectation expectations)
      (assert (> 2 (count :return-type expectations
                          :key (lambda (ex) (keywordify (car ex)))))
              ()
              "There are more than one return type expectations ~
             provided when there should only be one.")
      
      ;; Parse the expectations.
      (mapc #'parse-safe-expectation expectations)
      (mapc #'parse-speedy-expectation expectations)
      
      ;; Construct the policy form.
      `(policy-if
        ,policy
        ;; Speedy version (policy is satisfied).
        ,(if (null expectations)
             `(progn ,@body)
             (let ((contents
                     (cond
                       ((eql return-types :not-provided) body)
                       ((= 1 (length return-types))
                        (list `(the ,@return-types (progn ,@body))))
                       (t (list `(the (values ,@return-types) (progn ,@body)))))))
               ;; XXX FIXME: MAKE THE OUTPUT BETTER
               (if local-declarations
                   `(locally (declare ,@local-declarations)
                      ,@contents)
                   `(progn ,@contents))))

        ;; Safe version (policy is not satisfied).
        ,(if (eql :not-provided return-types)
             `(progn
                ,@preamble-forms
                ,@body)
             (let ((result (gensym "RESULT-"))
                   (i      (gensym "I-"))
                   (type   (gensym "TYPE-"))
                   (value  (gensym "VALUE-")))
               (if (= 1 (length return-types))
                   ;; The simple case of one return type.
                   `(progn
                      ,@preamble-forms
                      (let ((,result (progn ,@body)))
                        (check-type ,result ,@return-types)
                        ,result))
                   
                   `(progn
                      ,@preamble-forms
                      (let ((,result (multiple-value-list (progn ,@body))))
                        (assert (= ,(length return-types)
                                   (length ,result))
                                ()
                                "Expected ~D values to get returned. Got ~D."
                                ,(length return-types)
                                (length ,result))
                        ,@(loop :for i :from 0
                                :for type :in return-types
                                :collect `(unless (typep (nth ,i ,result))
                                            (error 'simple-type-error
                                                   :format-control ,(format nil "The ~:R value returned, ~~S, is not of type ~S."
                                                                            (1+ i)
                                                                            type) 
                                                   :format-arguments (list (nth ,i ,result))
                                                   :datum (nth ,i ,result)
                                                   :expected-type ',type)))
                        #+#:ignore
                        (loop :for ,i :from 1
                              :for ,value :in ,result
                              :for ,type :in ',return-types
                              :do (unless (typep ,value ,type)
                                    (error 'simple-type-error
                                           :format-control "The ~:R value returned, ~S, is not of type ~S."
                                           :format-arguments (list ,i ,value ,type)
                                           :datum ,value
                                           :expected-type ,type)))
                        (values-list ,result))))))))))
