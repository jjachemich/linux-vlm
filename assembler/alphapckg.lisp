;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10; Lowercase: Yes -*-

(defpackage ALPHA-AXP-INTERNALS
  (:nicknames AXPI)
  #+Genera (:use SCL LISP)
  #-Genera (:use COMMON-LISP)
  (:shadow AND)
  #+OpenMCL (:import-from CCL LSH DEFSUBST STACK-LET CIRCULAR-LIST))

