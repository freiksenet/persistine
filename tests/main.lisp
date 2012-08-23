(cl:defpackage #:persistine.tests
               (:use #:cl #:fiveam))
(cl:in-package #:persistine.tests)

(def-suite all)

(def-suite structures :in all
  :description "Tests related to concrete data structure implementation.")
(def-suite interface :in all
  :description "Tests related to generic API.")
