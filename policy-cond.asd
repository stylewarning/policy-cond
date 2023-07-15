;;;; policy-cond.asd
;;;;
;;;; Copyright (c) 2014 Robert Smith

(asdf:defsystem #:policy-cond
  :description "Tools to insert code based on compiler policy."
  :long-description "POLICY-COND provides tools to insert and execute code based on one's compiler's OPTIMIZE policy. It also contains a contract-like notion of 'expectations', which allow dynamic checking or inclusion of various things to happen depending on compiler policy."
  :author "Robert Smith <quad@symbo1ics.com>"
  :maintainer "Robert Smith <quad@symbo1ics.com>"
  :license "BSD 3-clause (See LICENSE)"
  :depends-on ((:feature :sbcl
                :sb-cltl2) ; An SBCL contrib enacapsulated via ASDF
               (:feature (:not (:or :sbcl :lispworks :cmucl :ccl :allegro))
                :cl-environments))
  :serial t
  :components ((:static-file "LICENSE.txt")
               (:file "package")
               (:file "policy-cond")
               (:file "expectations")
               (:file "policy")))
