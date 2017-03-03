;;; Package defs for the Scheme Underground sorting package,
;;; in the Scheme 48 module language.

;;; This code is
;;;     Copyright (c) 1998 by Olin Shivers.
;;; The terms are: You may do as you please with this code, as long as
;;; you do not delete this notice or hold me responsible for any outcome
;;; related to its use.
;;;
;;; Blah blah blah. Don't you think source files should contain more lines
;;; of code than copyright notice?

;;; The general sort package:

(define-structure sorting sorting-interface
  (open scheme
	list-merge-sort
	vector-heap-sort
	vector-merge-sort
	vector-quick-sort
	sorted
	delete-neighbor-duplicates)
  (files sort)
  (optimize auto-integrate))

(define-structure sorted sorted-interface
  (open scheme
	vector-utils)
  (files sortp)
  (optimize auto-integrate))

(define-structure delete-neighbor-duplicates delete-neighbor-duplicates-interface
  (open scheme
	vector-utils)
  (files delndups)
  (optimize auto-integrate))

(define-structure binary-searches binary-searches-interface
  (open scheme
	vector-utils)
  (files vbinsearch))

(define-structure list-merge-sort list-merge-sort-interface
  (open scheme
	(subset exceptions (assertion-violation)))
  (files lmsort)
  (optimize auto-integrate))

(define-structure vector-merge-sort vector-merge-sort-interface
  (open scheme
	vector-utils
	vector-insertion-sort-internal)
  (files vmsort)
  (optimize auto-integrate))

(define-structure vector-heap-sort vector-heap-sort-interface
  (open scheme
	vector-utils)
  (files vhsort)
  (optimize auto-integrate))

(define-structures ((vector-insertion-sort vector-insertion-sort-interface)
		    (vector-insertion-sort-internal
		     vector-insertion-sort-internal-interface))
  (open scheme
	vector-utils)
  (files visort)
  (optimize auto-integrate))

(define-structure vector-quick-sort vector-quick-sort-interface
  (open scheme
	vector-utils
	vector-insertion-sort-internal)
  (files vqsort2))

(define-structure vector-quick-sort3 vector-quick-sort3-interface
  (open scheme
	vector-utils
	vector-insertion-sort-internal)
  (files vqsort3))

(define-structure vector-utils (export vector-copy
				       vector-portion-copy
				       vector-portion-copy!
				       vector-start+end
				       vectors-start+end-2)
  (open scheme)
  (files vector-util))

