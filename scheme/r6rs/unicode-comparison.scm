; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-n-ary-comparison char-ci=? char? char-foldcase prim:char=?)
(define-n-ary-comparison char-ci<? char? char-foldcase prim:char<?)
(define-n-ary-comparison char-ci>? char? char-foldcase prim:char>?)
(define-n-ary-comparison char-ci<=? char? char-foldcase prim:char<=?)
(define-n-ary-comparison char-ci>=? char? char-foldcase prim:char>=?)

(define-n-ary-comparison string-ci=? string? string-foldcase prim:string=?)
(define-n-ary-comparison string-ci<? string? string-foldcase prim:string<?)
(define-n-ary-comparison string-ci>? string? string-foldcase prim:string>?)
(define-n-ary-comparison string-ci<=? string? string-foldcase prim:string<=?)
(define-n-ary-comparison string-ci>=? string? string-foldcase prim:string>=?)
