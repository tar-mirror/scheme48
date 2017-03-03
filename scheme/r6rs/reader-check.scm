; Part of Scheme 48 1.9.  See file COPYING for notices and license.

; Authors: Mike Sperber

(define-test-suite r6rs-reader-tests)

(define-test-case tokens r6rs-reader-tests
  (check
   (get-datum (make-string-input-port "-123")) => -123)
  (check
   (get-datum (make-string-input-port "+123")) => 123)
  (check
   (get-datum (make-string-input-port "...")) => '...)
  (check
   (get-datum (make-string-input-port "(...)")) => '(...))
  (check
   (get-datum (make-string-input-port "(... foo ... baz)")) => '(... foo ... baz))
  (check-exception
   (get-datum (make-string-input-port "..")))
  (check
   (get-datum (make-string-input-port ".5")) => 0.5)
  (check
   (get-datum (make-string-input-port "(1 2 3)")) => '(1 2 3))
  (check-exception
   (get-datum (make-string-input-port "(1 2 3]")))
  (check
   (get-datum (make-string-input-port "foo")) => 'foo)
  (check
   (get-datum (make-string-input-port "fOo")) => (string->symbol "fOo"))
  (check
   (get-datum (make-string-input-port "[1 2 3]")) => '(1 2 3))
  (check
   (get-datum (make-string-input-port "#\\linefeed")) => (integer->char 10))
  (check
   (get-datum (make-string-input-port "#\\x578")) => (integer->char #x578))
  (check
   (get-datum (make-string-input-port "\"\\a\\b\\t\\n\\v\\f\\r\\\"\\\\\""))
   => (list->string (map integer->char '(7 8 9 #xA #xB #xC #xD #x22 #x5c))))
  (check
   (get-datum (make-string-input-port "\"\\x578;\\x123;\""))
   => (list->string (map integer->char '(#x578 #x123))))
  (check-exception
   (get-datum (make-string-input-port "\"\\x578;\\x123\"")))
  (check-exception
   (get-datum (make-string-input-port "\"\\x578\\x123\"")))
  (check-exception
   (get-datum (make-string-input-port "#\\Alarm")))
  (check
   (get-datum (make-string-input-port "h\\x65;llo")) => 'hello)
  (check-exception
   (get-datum (make-string-input-port "h\\x65llo")))
  (check
   (get-datum (make-string-input-port "\\x2e;reader.")) => (string->symbol ".reader."))
  (check
   (get-datum (make-string-input-port "'foo")) => '(quote foo))
  (check
   (get-datum (make-string-input-port "`foo")) => '(quasiquote foo))
  (check
   (get-datum (make-string-input-port ",foo")) => '(unquote foo))
  (check
   (get-datum (make-string-input-port ",@foo")) => '(unquote-splicing foo))
  (check
   (get-datum (make-string-input-port "#'foo")) => '(syntax foo))
  (check
   (get-datum (make-string-input-port "#`foo")) => '(quasisyntax foo))
  (check
   (get-datum (make-string-input-port "#,foo")) => '(unsyntax foo))
  (check
   (get-datum (make-string-input-port "#,@foo")) => '(unsyntax-splicing foo))
  (check
   (get-datum (make-string-input-port "(1 #| foo bar |# 2 3)")) => '(1 2 3))
  (check
   (get-datum (make-string-input-port "(1 #| foo #| bar |# |# 2 3)")) => '(1 2 3))
  (check
   (get-datum (make-string-input-port "(1 #;(foo bar baz) 2 3)")) => '(1 2 3))
  (check
   (get-datum (make-string-input-port "->foo")) => (string->symbol "->foo"))
  (check
   (get-datum (make-string-input-port "#vu8(1 2 3 4 5)")) (=> blob=?) (u8-list->blob '(1 2 3 4 5)))
  (check
   (get-datum (make-string-input-port "(#t #f #b1001 #T #F #B1001)")) => '(#t #f 9 #t #f 9))
  (check-exception
   (get-datum (make-string-input-port "@")))
  (check
   (get-datum (make-string-input-port "a@")) => 'a@)

  (check
   (get-datum (make-string-input-port (string (integer->char #xa0)))) => (eof-object))
  (check
   (get-datum (make-string-input-port (string (integer->char #xa1)))) =>
   (string->symbol (string (integer->char #xa1))))
  (check-exception
   (get-datum (make-string-input-port "(#\\Z#\\F)")))
  (check
   (get-datum (make-string-input-port "(#\\Z #\\F)")) => '(#\Z #\F))
  (check-exception
   (get-datum (make-string-input-port "->#")))
  (check-exception
   (get-datum (make-string-input-port "(a#b)")))
  (check-exception
   (get-datum (make-string-input-port "(a,b)"))) ; missing delimiter
  (check
   (get-datum (make-string-input-port "(a ,b)")) => '(a (unquote b)))
  (check-exception
   (get-datum (make-string-input-port "(#\\A,b)"))) ; missing delimiter
  (check
   (get-datum (make-string-input-port "(#\\A ,b)")) => '(#\A (unquote b)))
  (check-exception
   (get-datum (make-string-input-port "(#\\t,b)")))
  (check
   (get-datum (make-string-input-port "(#\\t ,b)")) => '(#\t (unquote b)))
  (check-exception
   (get-datum (make-string-input-port "(#t#f)")))
  (check
   (get-datum (make-string-input-port "(#t #f)")) => '(#t #f))

  (check
   (get-datum (make-string-input-port "#!r6rs")) => (eof-object))
  (check
   (get-datum (make-string-input-port "#!r6rs ")) => (eof-object))
  (check
   (get-datum (make-string-input-port "#!r6rs a")) => 'a))

(define (single-character-passes lo hi)
  (do ((i lo (+ 1 i))
       (r '()
	  (guard
	   (_
	    (else r))
	   (get-datum (make-string-input-port (string (integer->char i))))
	   (cons i r))))
      ((> i hi)
       (reverse r))))

(define-test-case single-character-passes r6rs-reader-tests
  (check (single-character-passes 0 255)
	 => '(9 10 11 12 13 32 33 36 37 38 42 43 45 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 94 95 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 126 133 160 161 162 163 164 165 166 167 168 169 170 172 174 175 176 177 178 179 180 181 182 183 184 185 186 188 189 190 191 192 193 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 215 216 217 218 219 220 221 222 223 224 225 226 227 228 229 230 231 232 233 234 235 236 237 238 239 240 241 242 243 244 245 246 247 248 249 250 251 252 253 254 255)))

(define (count-single-character-passes lo hi)
  (do ((i lo (+ 1 i))
       (r 0
	  (guard
	   (_
	    (else r))
	   (get-datum (make-string-input-port (string (integer->char i))))
	   (+ 1 r))))
      ((> i hi)
       r)))

(define-test-case count-single-character-passes r6rs-reader-tests
  ;; this takes about a minute on Mike's machine
  (check (count-single-character-passes 0 #x10FFFF) => 235735))
