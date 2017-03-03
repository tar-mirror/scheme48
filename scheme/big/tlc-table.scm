; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Marcus Crestani, Robert Ransom, Harald Glab-Phlak

;; transport link cells hash table
;; Ghuloum, Dybvig 2007

;; TODO
;; - shall the hashtable grow/shrink over time?

;; record type for table

(define-record-type tlc-table :tlc-table
  (really-make-tlc-table buckets-size buckets hash-function 
			 equivalence-function tconc
			 count loc)
  tlc-table?
  (buckets-size tlc-table-buckets-size set-tlc-table-buckets-size!)
  (buckets tlc-table-buckets set-tlc-table-buckets!)
  (hash-function tlc-table-hash-function)               ; hash function
  (equivalence-function tlc-table-equivalence-function) ; equivalence function
  (tconc tlc-table-tconc)             ; to track the links that need rehashing
  (count tlc-table-count set-tlc-table-count!)   ; number of elements in table
  (loc tlc-table-loc set-tlc-table-loc!))   ; doubly-linked list with all TLCs

;; record type for value

(define-record-type tlc-value :tlc-value
  (make-tlc-value value prev-tlc next-tlc)
  tlc-value?
  (value tlc-value-value set-tlc-value-value!)
  (prev-tlc tlc-value-prev-tlc set-tlc-value-prev-tlc!)
  (next-tlc tlc-value-next-tlc set-tlc-value-next-tlc!))
  
;; minimal size of a tlc table
(define *tlc-table-min-size* 1)

;; default size if no size is given
(define *tlc-table-default-init-size* 20)

;; initialize buckets

(define (tlc-table-initialize-buckets! buckets)
  (let fill-with-index! ((vector buckets)
			 (n (- (vector-length buckets) 1)))
      (if (>= n 0)
	  (begin
	    (vector-set! vector n n)
	    (fill-with-index! vector (- n 1))))))
  
;; smart constructor

(define (make-tlc-table-internally size hash-function 
				   equiv-function use-tconc-queue?)
  (let* ((size (max size *tlc-table-min-size*))
	 (buckets (make-vector size))
	 (tconc (and use-tconc-queue? (make-tconc-queue))))
    (tlc-table-initialize-buckets! buckets)
    (really-make-tlc-table size buckets hash-function equiv-function tconc 0 #f)))

;; default hash functions

(define (tlc-table-default-eq-hash-function object)
  (memory-status (enum memory-status-option pointer-hash) object))

(define (tlc-table-default-eqv-hash-function x)
  (let recur ((x x)
              (budget 16))
    (cond
     ((number? x)
      (number-hash x))  ; imported from general-tables
     ((string? x)
      (table-string-hash x))  ; imported from general-tables
     ((symbol? x)
      (table-symbol-hash x))  ; imported from general-tables
     (else
      (memory-status (enum memory-status-option pointer-hash) x)))))

;; adjust results of hash function to table size

(define (tlc-table-hash-value size value)
  (let ((v (remainder value size)))
    (if (< v 0)
	(- v)
	v)))

(define (tlc-table-calculate-hash table object)
  (tlc-table-hash-value (tlc-table-buckets-size table)
			((tlc-table-hash-function table) object)))

;; access link chains

(define (set-tlc-table-entry! table index link)
  (vector-set! (tlc-table-buckets table) index link))

(define (tlc-table-entry table index)
  (vector-ref (tlc-table-buckets table) index))

;; insert links

(define (tlc-table-insert-link table link)
  (let* ((key (transport-link-cell-key link))
	 (index (tlc-table-calculate-hash table 
					  (transport-link-cell-key link))))
    (set-transport-link-cell-next! link (tlc-table-entry table index))
    (set-tlc-table-entry! table index link)))

(define (tlc-table-add table key value)
  (let* ((tlc-value (make-tlc-value value #f (tlc-table-loc table)))
	 (link (make-transport-link-cell key tlc-value 
					 (tlc-table-tconc table) #f)))
    (tlc-table-insert-link table link)
    (if (tlc-table-loc table)
	(set-tlc-value-prev-tlc! (transport-link-cell-value
				  (tlc-table-loc table)) link))
    (set-tlc-table-loc! table link)
    (set-tlc-table-count! table (+ (tlc-table-count table) 1))))

;; get index of link chain

(define (tlc-table-index-of-link x)
  (if (number? x)
      x
      (tlc-table-index-of-link (transport-link-cell-next x))))

;; delete links

(define (tlc-table-delete-link table link)
  (let* ((index (tlc-table-index-of-link link))
	 (chain (tlc-table-entry table index)))
    (letrec ((delete-loop 
	      (lambda (chain)
		(and (transport-link-cell? chain)
		     (let ((x (transport-link-cell-next chain)))
		       (if (transport-link-cell? x)
			   (if (eq? x link)
			       (set-transport-link-cell-next!
				chain (transport-link-cell-next x))
			       (delete-loop x))
			   (set-transport-link-cell-next! chain x)))))))
      (if (eq? chain link)
	  (set-tlc-table-entry! table index
				(transport-link-cell-next link))
	  (delete-loop chain))
      (let* ((tlc-value (transport-link-cell-value link))
	     (prev-tlc (tlc-value-prev-tlc tlc-value))
	     (next-tlc (tlc-value-next-tlc tlc-value)))
	(if prev-tlc
	    (set-tlc-value-next-tlc! (transport-link-cell-value prev-tlc) next-tlc)
	    (set-tlc-table-loc! table next-tlc))
	(if next-tlc
	    (set-tlc-value-prev-tlc! (transport-link-cell-value next-tlc) prev-tlc)))
      (set-tlc-table-count! table (- (tlc-table-count table) 1)))))

;; lookup

(define (tlc-table-direct-lookup table key)
  (let ((index (tlc-table-calculate-hash table key)))
    (let lookup ((x (tlc-table-entry table index)))
      (and (transport-link-cell? x)
	   (if ((tlc-table-equivalence-function table) (transport-link-cell-key x) key)
	       x
	       (lookup (transport-link-cell-next x)))))))

(define (tlc-table-rehash-link table link)
  (tlc-table-delete-link table link)
  (tlc-table-add table 
		 (transport-link-cell-key link) 
		 (tlc-value-value (transport-link-cell-value link))))

(define (tlc-table-rehash-lookup table key)
  (let ((tconc (tlc-table-tconc table)))
    (let tconc-dequeue-loop ()
      (and tconc (not (tconc-queue-empty? tconc))
	   (let ((link (tconc-queue-dequeue! tconc)))
	     (tlc-table-rehash-link table link)
	     (if ((tlc-table-equivalence-function table) (transport-link-cell-key link) key)
		 link
		 (tconc-dequeue-loop)))))))

(define (tlc-table-lookup-link table key)
  (or (tlc-table-direct-lookup table key)
      (tlc-table-rehash-lookup table key)))

(define (tlc-table-rehash-and-clean-tconc-queue table key)
  (let ((tconc (tlc-table-tconc table)))
    (let tconc-dequeue-loop ()
      (and tconc (not (tconc-queue-empty? tconc))
	   (let ((link (tconc-queue-dequeue! tconc)))
	     (if ((tlc-table-equivalence-function table) (transport-link-cell-key link) key)
		 link
		 (begin
		   (tlc-table-rehash-link table link)
		   (tconc-dequeue-loop))))))))

;; DELETING FROM A TLC TABLE IS DIFFICULT:
;; There are rare occasions where a link is enqueued to the tconc
;; queue during garbage collection that is hashed into the same bucket
;; as before.  So, strictly speaking, there is no need for the link to
;; go into the tconc queue because a direct lookup finds it anyway.
;; But if the user really wants to delete a link, we have to make sure
;; that it is removed from the tconc queue so that a later lookup will
;; not resurrect the link.  Thus, if the tlc's tconc field is #f, the
;; tlc is in the tconc queue and we first walk the tconc queue and 
;; rehash all the links until we find the link we want to delete.
;;
;; This may make the removal of an tlc-table entry very expensive,
;; because worst case all links in the tconc queue are rehashed
;; whenever the user deletes an element from the tlc table.
;;
;; In even more rare circumstances, a deleted link may resurrect this
;; way: If a garbage collection happens during the deletion of a link
;; (i.e. while traversing a bucket's link list), the collector may
;; enqueue the link to the tconc queue just before the link is
;; deleted from the link list.  To prevent this from happening, we set
;; the link's tconc field to #f, so that the collector will not try to
;; enqueue it.
;;
;; For non-deleting lookups it does not matter if the link is still in
;; the tconc.  At some point in time, the link will be rehashed to the
;; same bucket as it was before.  This is unneeded but way cheaper
;; than checking and acting to prevent such a situation.

(define (tlc-table-lookup-link-for-deletion table key)
  (let ((link (tlc-table-direct-lookup table key)))
    (if (and link (tlc-table-tconc table) (transport-link-cell-tconc link))
	(begin
	  (set-transport-link-cell-tconc! link #f)
	  link)
	  (tlc-table-rehash-and-clean-tconc-queue table key))))

;; exported functions below

;; constructors

(define (make-non-default-tlc-table hash-function equiv size use-tconc-queue?)
  (make-tlc-table-internally size hash-function equiv use-tconc-queue?))

(define make-eq-tlc-table
  (opt-lambda ((size *tlc-table-default-init-size*))
	      (make-non-default-tlc-table tlc-table-default-eq-hash-function eq? size #t)))

(define make-tlc-table make-eq-tlc-table)

(define make-eqv-tlc-table
  (opt-lambda ((size *tlc-table-default-init-size*))
	      (make-non-default-tlc-table tlc-table-default-eqv-hash-function eqv? size #t)))

;; size

(define tlc-table-size tlc-table-count)

;; lookup

(define (tlc-table-ref table key not-found)
  (let ((x (tlc-table-lookup-link table key)))
    (if x
	(tlc-value-value (transport-link-cell-value x))
	not-found)))

;; set

(define (assert-immutable table origin)
  (and (immutable? table)
       (assertion-violation origin
			    "immutable argument"
			    table)))

(define (tlc-table-set! table key value)
  (assert-immutable table 'tlc-table-set!)
  (let ((x (tlc-table-lookup-link table key)))
    (if x
	(let ((tlc-value (transport-link-cell-value x)))
	  (set-tlc-value-value! tlc-value value))
	(tlc-table-add table key value))))

;; delete

(define (tlc-table-delete! table key not-found)
  (assert-immutable table 'tlc-table-delete!)
  (let ((x (tlc-table-lookup-link-for-deletion table key)))
    (if x
	(tlc-table-delete-link table x)
	not-found)))

;; contains?

(define (tlc-table-contains? table key)
  (and (tlc-table-lookup-link table key) #t))

;; update

(define (tlc-table-update! table key proc not-found)
  (assert-immutable table 'tlc-table-update!)
  (let ((x (tlc-table-lookup-link table key)))
    (if x
	(let ((tlc-value (transport-link-cell-value x)))
	  (set-tlc-value-value! 
	   tlc-value
	   (proc (tlc-value-value tlc-value))))
	not-found)))

;; copy

(define (make-tlc-table-immutable! table)
   (and (tlc-table-tconc table)
	(assertion-violation 'make-tlc-table-immutable!
			     "tlc tables that need tconc queues cannot be made immutable"))
  (let loop ((tlc (tlc-table-loc table)))
    (if tlc
	(begin
	  (make-immutable! (transport-link-cell-value tlc))
	  (make-immutable! (transport-link-cell-key tlc))
	  (make-immutable! tlc)
	  (loop (tlc-value-next-tlc (transport-link-cell-value tlc))))))
  (make-immutable! (tlc-table-buckets table))
  (make-immutable! table))

(define tlc-table-copy
  (opt-lambda
   (table (mutable? #f))
   (and (not mutable?) (tlc-table-tconc table)
	(assertion-violation 'tlc-table-copy
			     "tlc tables that need tconc queues cannot be copied to immutable tables"))
   (let ((hash-function (tlc-table-hash-function table))
	 (equiv (tlc-table-equivalence-function table))
	 (size (tlc-table-count table))
	 (use-tconc-queue? (tlc-table-tconc table)))
     (let ((copy (make-non-default-tlc-table hash-function equiv size use-tconc-queue?)))
       (let loop ((tlc (tlc-table-loc table)))
	 (if tlc
	     (begin 
	       (tlc-table-add copy
			      (transport-link-cell-key tlc)
			      (tlc-value-value (transport-link-cell-value tlc)))
	       (loop (tlc-value-next-tlc (transport-link-cell-value tlc))))))
       (and (not mutable?) (make-tlc-table-immutable! copy))
       copy))))

;; clear

(define (tlc-table-clear! table)
  (assert-immutable table 'tlc-table-clear!)
  (tlc-table-initialize-buckets! (tlc-table-buckets table))
  (if (tlc-table-tconc table)
      (tconc-queue-clear! (tlc-table-tconc table)))
  (set-tlc-table-count! table 0)
  (set-tlc-table-loc! table #f))

;; resize

;; at the moment if and really only if the table is empty

(define (tlc-table-resize! table size)
  (assert-immutable table 'tlc-table-resize!)
  (let ((capacity (vector-length (tlc-table-buckets table)))
	(buckets (tlc-table-buckets table))
	(count (tlc-table-count table)))
    (if (eq? count 0)
	(begin (set-tlc-table-buckets! table (make-vector size))
	       (set-tlc-table-buckets-size! table size)
	       (tlc-table-initialize-buckets! (tlc-table-buckets table))
	       (set-tlc-table-loc! table #f) #t)
	#f)))

;; keys

(define (tlc-table-keys table)
  (let ((keys (make-vector (tlc-table-count table))))
    (let loop ((tlc (tlc-table-loc table))
	       (count 0))
      (if tlc
	  (begin
	    (vector-set! keys count (transport-link-cell-key tlc))
	    (loop
	     (tlc-value-next-tlc (transport-link-cell-value tlc))
	     (+ count 1))))
      keys)))

;; keys & values

(define (tlc-table-entries table)
  (let ((keys (make-vector (tlc-table-count table)))
	(vals (make-vector (tlc-table-count table))))
    (let loop ((tlc (tlc-table-loc table))
	       (count 0))
      (if tlc
	  (begin
	    (vector-set! keys count (transport-link-cell-key tlc))
	    (vector-set! vals count (tlc-value-value
				     (transport-link-cell-value tlc)))
	    (loop
	     (tlc-value-next-tlc (transport-link-cell-value tlc))
	     (+ count 1))))
      (values keys vals))))

;; utility function to detect eq? and eqv? tables (so their hash
;; functions can be hidden from R6RS code)

(define (tlc-table-has-tconc-queue? table)
  (and (tlc-table-tconc table)
       #t))

;; exported hash functions

(define (string-ci-hash value)
  (let ((converted (string-foldcase value))) ;; get sring-fold... from r6rs impl
    (string-hash converted)))

(define (equal-hash object)
  (datum-hash object))

;; debugging

(define (tlc-table-distribution table)
  (let loop-table ((n (- (tlc-table-buckets-size table) 1))
		   (distribution '()))
    (let ((count
	   (let loop-chain ((x (tlc-table-entry table n))
			    (count 0))
	     (if (transport-link-cell? x)
		 (loop-chain (transport-link-cell-next x) (+ count 1))
		 count)))
	  (count-tconc
	   (let ((tconc (tlc-table-tconc table)))
	     (and tconc (pair? tconc)
		  (let loop-tconc ((x (car tconc))
				   (count 0))
		    (if (or (eq? x (cdr tconc))
			    (not (pair? x)))
			count
			(loop-tconc (cdr x) (+ count 1)))))))
	  (count-loc
	   (let loop ((tlc (tlc-table-loc table))
		      (count 0))
	     (if tlc
		 (loop (tlc-value-next-tlc (transport-link-cell-value tlc))
		       (+ count 1))
		 count))))
      (if (> n 0)
	  (loop-table (- n 1) (cons (cons n count) distribution))
	  (list (cons 'tconc count-tconc)
		(cons 'count (tlc-table-count table))
		(cons 'loc count-loc)
		(cons 'buckets
		      (cons (cons n count) distribution)))))))
