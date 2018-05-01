;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

(defpackage POWERPC-INTERNALS
  (:nicknames PPCI)
  #+Genera (:use SCL LISP)
  #-Genera (:use COMMON-LISP)
  (:shadow AND OR)
  #+OpenMCL (:import-from CCL LSH DEFSUBST STACK-LET CIRCULAR-LIST))

