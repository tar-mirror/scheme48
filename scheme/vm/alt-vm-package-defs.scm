; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Richard Kelsey, Jonathan Rees, David Frese, Mike Sperber

; These are used to compile the GC separately from the VM.  It has new
; definitions of the VM structures that the GC uses, with all of the values
; defined as externals.  We only need to define those values that the GC
; actually uses.

(define-structure interpreter-gc interpreter-gc-interface
  (open prescheme)
  (begin
    (define s48-gc-root
      (external "s48_gc_root" (=> () null)))
    (define s48-post-gc-cleanup
      (external "s48_post_gc_cleanup" (=> (boolean boolean) null)))))

(define-structure symbols (export s48-symbol-table)
  (open prescheme)
  (begin
    (define s48-symbol-table
      (external "s48_symbol_table" (=> () integer)))))

(define-structure vmio (export s48-channels s48-channel-count)
  (open prescheme)
  (begin
    (define s48-channels
      (external "s48_channels" (=> () (^ integer))))
    (define s48-channel-count
      (external "s48_channel_count" (=> () integer)))))

(define-structure shared-bindings-access shared-bindings-access-interface
  (open prescheme)
  (begin
    (define s48-imported-bindings
      (external "s48_imported_bindings" (=> () integer)))
    (define s48-exported-bindings
      (external "s48_exported_bindings" (=> () integer)))))

