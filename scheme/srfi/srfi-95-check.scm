; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcus Crestani

;;; Test suite for SRFI-95

(define-test-suite srfi-95-tests)

(define list-unsorted (list 2 32 42 23 1 2 74 3 65))
(define list-sorted (list 1 2 2 3 23 32 42 65 74))
(define list-sorted-1 (list 32 42 65 74))
(define list-sorted-2 (list 1 2 2 3 23))
(define list-< <)

(define vector-unsorted (list->vector list-unsorted))
(define vector-sorted (list->vector list-sorted))
(define vector-sorted-1 (list->vector list-sorted-1))
(define vector-sorted-2 (list->vector list-sorted-2))
(define vector-< <)

(define array-unsorted (list->array 1 '#() list-unsorted))
(define array-sorted (list->array 1 '#() list-sorted))
(define array-< <)

(define-test-case sorted? srfi-95-tests
  (check (sorted? list-sorted list-<))
  (check (not (sorted? list-unsorted list-<)))
  (check (sorted? vector-sorted vector-<))
  (check (not (sorted? vector-unsorted vector-<)))
  (check (sorted? array-sorted array-<))
  (check (not (sorted? array-unsorted array-<))))

(define-test-case sort srfi-95-tests
  (check (sort list-sorted list-<) => list-sorted)
  (check (sort list-unsorted list-<) => list-sorted)
  (check (sort vector-sorted vector-<) => vector-sorted)
  (check (sort vector-unsorted vector-<) => vector-sorted)
  (check (array->vector (sort array-sorted array-<))
	 => (array->vector array-sorted))
  (check (array->vector (sort array-unsorted array-<))
	 => (array->vector array-sorted)))

(define-test-case sort! srfi-95-tests
  (check (sort! list-sorted list-<) => list-sorted)
  (check (sort! list-unsorted list-<) => list-sorted)
  (check (sort! vector-sorted vector-<) => vector-sorted)
  (check (sort! vector-unsorted vector-<) => vector-sorted)
  (check (array->vector (sort! array-sorted array-<))
	 => (array->vector array-sorted))
  (check (array->vector (sort! array-unsorted array-<))
	 => (array->vector array-sorted)))

(define-test-case merge srfi-95-tests
  (check (merge list-sorted-1 list-sorted-2 list-<) => list-sorted)
  (check (merge list-sorted-2 list-sorted-1 list-<) => list-sorted)
  (check (merge vector-sorted-1 vector-sorted-2 vector-<) => vector-sorted)
  (check (merge vector-sorted-2 vector-sorted-1 vector-<) => vector-sorted))

(define-test-case merge! srfi-95-tests
  (check (merge! list-sorted-1 list-sorted-2 list-<) => list-sorted)
  (check (merge! vector-sorted-1 vector-sorted-2 vector-<) => vector-sorted))
