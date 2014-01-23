;;;; policy-cond.asd

(asdf:defsystem #:policy-cond
  :serial t
  :description "A macro to insert code based on compiler policy."
  :author "Robert Smith <quad@symbo1ics.com>"
  :maintainer "Robert Smith <quad@symbo1ics.com>"
  :license "Public Domain"
  :components ((:file "package")
               (:file "policy-cond")
               (:file "policy")))

