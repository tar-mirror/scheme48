; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, Marcus Crestani, Mike Sperber,
; Martin Gasbichler


(define-structures ((vm-utilities vm-utilities-interface))
  (open prescheme)
  (files (util vm-utilities))
  (begin
    (define-syntax assert
      (lambda (exp rename compare)
    	0))
    ))

(define-structures ((external external-interface))
  (open prescheme)
  (begin
    (define extended-vm
      (external "s48_extended_vm" (=> (integer integer) integer)))
    (define external-call
      (external "s48_external_call" (=> (integer integer integer address)
					integer)))
    (define external-call-2
      (external "s48_external_call_2" (=> (integer integer integer address)
					integer)))
    (define schedule-interrupt 
      (external "s48_schedule_alarm_interrupt" (=> (integer) integer)))

    ;; implemented in C, wrapper around s48-dequeue-external-event/unsafe!
    (define dequeue-external-event!
      (external "s48_dequeue_external_event" (=> () integer boolean)))

    (define cheap-time
      (external "CHEAP_TIME" (=> () integer)))
    (define real-time 
      (external "s48_real_time" (=> () integer integer)))
    (define run-time 
      (external "s48_run_time" (=> () integer integer)))
    
    (define get-os-string-encoding
      (external "s48_get_os_string_encoding" (=> () (^ char))))

    (define host-architecture
      (external "S48_HOST_ARCHITECTURE" (^ char)))

    (define s48-call-native-procedure
      (external "s48_call_native_procedure" (=> (integer integer) integer)))
    (define s48-invoke-native-continuation
      (external "s48_invoke_native_continuation" (=> (integer integer) integer)))
    (define s48-jump-native
      (external "s48_jump_to_native_address" (=> (integer integer) integer)))
    (define s48-native-return
      (external "((long)&s48_native_return)" integer))

    (define get-proposal-lock!
      (external "GET_PROPOSAL_LOCK" (=> () null)))
    (define release-proposal-lock!
      (external "RELEASE_PROPOSAL_LOCK" (=> () null)))

    (define shared-ref
      (external "SHARED_REF" (=> (integer) integer)))
    (define real-shared-set!
      (external "SHARED_SETB" (=> (integer integer) null)))
    (define-syntax shared-set!
      (syntax-rules ()
	((shared-set! x v)
         (real-shared-set! x v))))

    ; for use in C functions usable from external code, defined as
    ; PreScheme procedures

    (define argument-type-violation
      ;; value
      (external "s48_argument_type_violation" (=> (integer) null)))

    (define range-violation
      ;; value, min, max
      (external "s48_range_violation" (=> (integer integer integer) null)))

    ; Lots of bignum stuff.  This should be moved to its own interface.
    (define export-key
      (external "s48_export_key" (=> (integer) integer)))
    (define external-bignum-make-cached-constants
      (external "s48_bignum_make_cached_constants" (=> () null)))
    (define external-bignum-add
      (external "(char *)s48_bignum_add" (=> (address address) address)))
    (define  external-bignum-subtract
      (external "(char *)s48_bignum_subtract" (=> (address address) address)))
    (define external-bignum-multiply
      (external "(char *)s48_bignum_multiply" (=> (address address) address)))
    (define external-bignum-quotient
      (external "(char *)s48_bignum_quotient" (=> (address address) address)))
    (define external-bignum-remainder
      (external "(char *)s48_bignum_remainder" (=> (address address) address)))
    (define external-bignum-divide
      (external "s48_bignum_divide" (=> (address address) 
 					boolean address address)))
    (define external-bignum-equal?
      (external "s48_bignum_equal_p" (=> (address address) boolean)))
    (define external-bignum-compare
      (external "s48_bignum_compare" (=> (address address) integer)))
    (define external-bignum-test
      (external "s48_bignum_test" (=> (address) integer)))
    (define external-bignum-negate
      (external "(char *) s48_bignum_negate" (=> (address) address)))
    (define external-bignum-arithmetic-shift
      (external "(char *) s48_bignum_arithmetic_shift" 
 		(=> (address integer) address)))
    (define external-bignum-bitwise-not
      (external "(char *) s48_bignum_bitwise_not"
 		(=> (address) address)))
    (define external-bignum-bit-count
      (external "s48_bignum_bit_count"
 		(=> (address) integer)))
    (define external-bignum-bitwise-and
      (external "(char *) s48_bignum_bitwise_and"
 		(=> (address address) address)))
    (define external-bignum-bitwise-ior
      (external "(char *) s48_bignum_bitwise_ior"
 		(=> (address address) address)))
    (define external-bignum-bitwise-xor
      (external "(char *) s48_bignum_bitwise_xor"
 		(=> (address address) address)))
    (define external-bignum-from-long
      (external "(char *) s48_long_to_bignum" (=> (integer) address)))
    (define external-bignum-from-unsigned-long
      (external "(char *) s48_ulong_to_bignum" (=> (unsigned-integer) address)))
    (define external-bignum->long
      (external "s48_bignum_to_long" (=> (address) integer)))
    (define external-bignum-fits-in-word?
      (external "s48_bignum_fits_in_word_p" (=> (address integer boolean) 
 						boolean)))

    ;; external call interface
    (define trace-external-calls
      (external "s48_trace_external_calls" (=> () null)))
    ))

(define-structures ((channel-io channel-interface)
		    (events event-interface))
  (open prescheme)
  (files (data ps-channel)))
