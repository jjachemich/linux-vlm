;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: ALPHA-AXP-INTERNALS; Base: 10; Lowercase: T -*-

#+Alpha-AXP-Emulator
(in-package "ALPHA-AXP-INTERNALS")

#+PowerPC-Emulator
(in-package "POWERPC-INTERNALS")

;;; *** If you change this file, be sure to change SYS:I-SYS;REV5-ERROR-TABLE.LISP ***

(defvar *all-conditions*
	'(%allocate-type-error 
	  %divide-bignum-step-not-fixnum-or-zero 
	  (%instance-reference-type-error :binary) 
	  (%instance-reference-type-error :three-argument) 
	  %memory-read-transport-and-fixnum-type-check 
	  %p-dpb-type-error 
	  aloc-non-object-array 
	  (array-access-type-check :binary) 
	  (array-access-type-check :three-argument) 
	  (array-leader-access-type-check :binary) 
	  (array-leader-access-type-check :three-argument) 
	  array-register-format-error 
	  array-register-format-error-or-subscript-bounds-error 
	  assoc-list-element-not-list 
	  bad-cdr-code-in-memory 
	  binary-arithmetic-operand-type-error 
	  binary-lexical-environment-type-error 
	  bind-locative-type-error 
	  binding-stack-overflow 
	  binding-stack-underflow 
	  block-read-binary-operation-transport-and-fixnum-type-check 
	  block-read-test-transport-and-fixnum-type-check 
	  block-read-transport-and-fixnum-type-check 
	  branch-dot-error 
	  byte-array-word-type-check 
	  car-cdr-list-type-error 
	  char-dpb-type-error 
	  char-ldb-type-error 
	  character-array-aset-type-error 
	  divide-by-zero 
	  divide-overflow 
	  fast-array-access-type-check 
	  fixnum-array-aset-type-error 
	  (flavor-search-mask-not-fixnum data-read) 
	  (flavor-search-table-pointer-not-locative data-read) 
	  frame-overflow 
	  (generic-search-table-entry-not-pc data-read) 
	  i-stage-error 
	  (illegal-full-word-instruction dtp-bound-location)
	  (illegal-full-word-instruction dtp-element-forward)
	  (illegal-full-word-instruction dtp-gc-forward)
	  (illegal-full-word-instruction dtp-header-forward)
	  (illegal-full-word-instruction dtp-header-i)
	  (illegal-full-word-instruction dtp-header-p)
	  (illegal-full-word-instruction dtp-logic-variable)
	  (illegal-full-word-instruction dtp-monitor-forward)
	  (illegal-full-word-instruction dtp-null)
	  (illegal-full-word-instruction dtp-one-q-forward)
	  illegal-instance-variable-index-from-memory 
	  (instance-flavor-table-contents-not-locative data-read) 
	  interpreter-table-contents-not-pc 
	  list-operation-tail-not-list		;+++ assoc/member/rgetf should generate this
	  mapping-table-index-out-of-bounds 
	  (memory-data-error bind-read) 
	  (memory-data-error bind-write) 
	  (memory-data-error cdr-read) 
	  (memory-data-error data-read) 
	  (memory-data-error data-write) 
	  (memory-data-error header-read) 
	  (memory-data-error scavenge) 
	  non-16-bit-character 
	  non-8-bit-character 
	  one-operand-fixnum-type-error 
	  one-operand-list-type-error 
	  (operand-1-type-error (dtp-locative)) 
	  operand-locative-type-error 
	  rplaca-rplacd-list-type-error		;+++ rplaca/rplacd should generate this
	  self-mapping-table-type-error 
	  self-type-error 
	  set-to-car-cdr-list-type-error 
	  setup-array-operand-not-array 
	  shallow-binding-operation-in-deep-binding-mode 
	  stack-blt-type-error 
	  subscript-bounds-error 
	  take-values-type-error 
	  three-operand-fixnum-type-error 
	  too-few-arguments 
	  too-many-arguments 
	  trap-on-exit 
	  two-operand-fixnum-type-error 
	  unary-arithmetic-operand-type-error 
	  unary-lexical-environment-type-error 
	  (unary-operand-type-error (dtp-odd-pc dtp-even-pc)) 
	  unknown-internal-register		;emulator only
	  unknown-double-float-op		;emulator only
	  ))

(defvar *vma-valid-conditions*
	'(assoc-list-element-not-list
	  bad-cdr-code-in-memory
	  byte-array-word-type-check
	  flavor-search-mask-not-fixnum
	  flavor-search-table-pointer-not-locative
	  generic-search-table-entry-not-pc
	  instance-flavor-table-contents-not-locative
	  interpreter-table-contents-not-pc
	  list-operation-tail-not-list
	  memory-data-error
	  shallow-binding-operation-in-deep-binding-mode
	  trap-on-exit))
