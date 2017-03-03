; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, David Frese,
; Mike Sperber, Martin Gasbichler

; Two interfaces that are shared with the Scheme code.  We can't share the
; structure definitions, but this is a start.

(define-interface vm-architecture-interface
  (export architecture-version
          (enum :syntax) ;so you don't have to remember to open enumerated
	  bits-used-per-byte byte-limit two-byte-limit
	  (interrupt :syntax)
	  interrupt-count
	  (memory-status-option :syntax)
	  (op :syntax)
	  op-count
	  opcode-arg-specs
	  (exception :syntax)
	  (stob :syntax)
	  stob-count
	  least-b-vector-type
	  stob-data
	  (time-option :syntax)
	  (channel-status-option :syntax)
	  (channel-parameter-option :syntax)
	  (port-status-options :syntax)
	  (current-port-marker :syntax)
	  (text-encoding-option :syntax)
	  (system-parameter-option :syntax)

	  maximum-stack-args
	  two-byte-nargs-protocol
	  two-byte-nargs+list-protocol
	  ignore-values-protocol
	  args+nargs-protocol
	  nary-dispatch-protocol
	  bottom-of-stack-protocol
	  call-with-values-protocol
	  big-stack-protocol
	  maximum-external-call-args
	  native-protocol-mask

	  default-stack-space
	  continuation-stack-size
	  available-stack-space

	  continuation-cells
	  continuation-cont-index
	  continuation-pc-index
	  continuation-code-index

	  gc-mask-size-offset
	  gc-mask-offset
	  
          exception-continuation-cells
	  exception-cont-size-index
	  exception-cont-pc-index
	  exception-cont-code-index
	  exception-cont-exception-index
	  exception-cont-instruction-size-index
          
          native-exception-continuation-cells
          native-exception-cont-size-index
          native-exception-cont-exception-index
          native-exception-cont-bc-pc-index
          native-exception-cont-bc-code-index
	  ))

; Data structures

(define-interface vm-data-interface
  (export bytes-per-cell bits-per-byte bits-per-cell addressing-units-per-cell
	  bytes->cells
	  cells->bytes
	  a-units->cells
	  cells->a-units
	  bytes->a-units

	  vm-eq?

	  (tag :syntax)
	  fixnum? immediate? header? stob?
	  tag-field-width
	  make-tag&immediate-type

	  enter-fixnum extract-fixnum
	  bits-per-fixnum greatest-fixnum-value least-fixnum-value
	  too-small-for-fixnum? too-big-for-fixnum? unsigned-too-big-for-fixnum?
	  descriptor->fixnum fixnum->stob
	  fixnum= fixnum< fixnum> fixnum<= fixnum>=
	  fixnum-bitwise-not fixnum-bitwise-and
	  fixnum-bitwise-ior fixnum-bitwise-xor
	  
	  (imm :syntax)
	  immediate-type-field-width
	  undefined?
	  true false vm-eof-object null unspecific-value unreleased-value quiescent
	  unbound-marker unassigned-marker
	  vm-boolean? false? enter-boolean extract-boolean
	  bytes-per-scalar-value-unit scalar-value-units->bytes
	  scalar-value-units->bytes bytes->scalar-value-units
	  vm-char? vm-char=? vm-char<?
	  enter-char extract-char scalar-value->vm-char vm-char->scalar-value

	  make-header header-type
	  header-type-field-width
	  header-immutable-bit-mask
	  header-length-in-bytes header-length-in-cells header-length-in-a-units
	  immutable-header? make-header-immutable
	  d-vector-header? b-vector-header?
	  max-stob-contents-size-in-cells

	  add-stob-tag remove-stob-tag
	  ))

