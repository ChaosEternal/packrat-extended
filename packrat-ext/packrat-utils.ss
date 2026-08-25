;; Packrat Parser Library Extended
;;
;; Copyright (c) 2017 Chaos Eternal <chaos@shlug.org>
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(library (packrat-ext packrat-utils)
  (export true
	  inverse
	  test-all
	  test-any
	  char-newline?
	  char-valid-hex?
	  char-nonzero-digit?
	  json-token-char?
	  jstring-body
	  jstring-body->string
	  token
	  )
  (import (packrat-ext packrat)
	  (rnrs))

  (define (true . dummy)
    #t)

  (define (inverse l)
    (lambda (x)
      (not (l x))))

  (define (test-all . pred)
    (lambda (c)
      (fold-left (lambda (r p) (and r (p c))) #t pred)))

  (define (test-any . pred)
    (lambda (c)
      (fold-left (lambda (r p) (or r (p c))) #f pred)))

  (define (char-newline? x)
    (memv x '(#\newline #\return)))

  (define (char-nonzero-digit? x)
    (and (char>=? x #\1) (char<=? x #\9)))

  ;; RFC 8259 has no delimiter production. true, false, null and a number are
  ;; whole tokens, and the surrounding grammar is what ends them, so a parser
  ;; that stops the moment a token's own characters run out reads `truex` as
  ;; `true` and `0x10` as `0` -- splitting one malformed token into a value
  ;; plus trailing junk.
  ;;
  ;; This is the set of characters that can continue such a token: the letters
  ;; that spell a literal or an exponent marker, the digits, and a number's
  ;; sign and decimal point. None of them may legitimately follow a value in
  ;; JSON, where only whitespace and the structural characters , ] } can, so a
  ;; negative lookahead on this predicate rejects the split without narrowing
  ;; any valid document.
  (define (json-token-char? c)
    (or (char-alphabetic? c)
	(char-numeric? c)
	(char=? c #\.)
	(char=? c #\+)
	(char=? c #\-)))


  (define char-valid-hex?
    (test-any
     char-numeric?
     (lambda (x)
       (and (char-ci<=? x #\F)
	    (char-ci>=? x #\A)))
     (lambda (x)
       (and (char-ci<=? x #\f)
	    (char-ci>=? x #\a)))))

  (define (a-char x)
    (lambda (y)
      (char=? x y)))

  (define (a-char-ci x)
    (lambda (y)
      (char-ci=? x y)))

  ;; RFC 8259 section 7 gives the set of characters a JSON string may carry
  ;; literally as
  ;;
  ;;   unescaped = %x20-21 / %x23-5B / %x5D-10FFFF
  ;;
  ;; so U+0000 through U+001F must be written as an escape, and the two holes
  ;; at %x22 and %x5C are quote and backslash, which have escapes of their own.
  ;; Everything from U+0020 up is allowed as-is, including U+007F, which JSON
  ;; does not treat as a control character.
  (define (json-unescaped-char? c)
    (and (char>=? c #\space)
	 (not (char=? c #\"))
	 (not (char=? c #\\))))

  (define jstring-body
    (packrat-parser any
		    (any ((c <- jstring-char s <- any) (cons c s))
			 ((c <- jstring-char) (cons c '()))
			 (() '()))
		    (jstring-char ((c <- (? json-unescaped-char?)) c)
				  (('#\\ '#\") #\")
				  (('#\\ '#\\) #\\)
				  (('#\\ '#\/) #\/)
				  (('#\\ '#\n) #\newline)
				  (('#\\ '#\b) #\backspace)
				  (('#\\ '#\f) #\page)
				  (('#\\ '#\r) #\return)
				  (('#\\ '#\t) #\tab)
				  (('#\\ '#\u a <- (? char-valid-hex?) b <- (? char-valid-hex?) c <- (? char-valid-hex?) d <- (? char-valid-hex?) )
				   (string->number (list->string (list a b c d)) 16)))))

  ;; jstring-body yields a MIXED list: a character for every literal character
  ;; and every single-character escape, and an exact integer for every \uXXXX
  ;; escape. The integer is deliberately left unconverted at the point it is
  ;; parsed, because a UTF-16 surrogate (D800-DFFF) is not a Unicode scalar
  ;; value and integer->char raises on it. RFC 8259 section 7 escapes a
  ;; non-BMP character as two consecutive \uXXXX escapes, so the halves can
  ;; only be joined once the whole body is known.
  ;;
  ;; That join cannot be a grammar alternative. An alternative matching two
  ;; consecutive \uXXXX escapes would also match a non-surrogate pair such as
  ;; \u0041\u0042, and ordered choice gives its action no way to backtrack.
  ;;
  ;; jstring-body->string is therefore the only supported way to turn a
  ;; jstring-body result into a string. Calling list->string on it is wrong.
  (define (%high-surrogate? n)
    (and (integer? n) (>= n #xD800) (<= n #xDBFF)))

  (define (%low-surrogate? n)
    (and (integer? n) (>= n #xDC00) (<= n #xDFFF)))

  (define (%surrogate-error message unit)
    (error 'json-read "JSON Parse Error"
	   (list 'json-parse-error message unit)))

  (define (jstring-body->string body)
    (let loop ((rest body) (acc '()))
      (cond
       ((null? rest)
	(list->string (reverse acc)))
       ((char? (car rest))
	(loop (cdr rest) (cons (car rest) acc)))
       ((%high-surrogate? (car rest))
	(let ((high (car rest))
	      (tail (cdr rest)))
	  (if (and (pair? tail) (%low-surrogate? (car tail)))
	      (loop (cdr tail)
		    (cons (integer->char
			   (+ #x10000
			      (* (- high #xD800) #x400)
			      (- (car tail) #xDC00)))
			  acc))
	      (%surrogate-error "high surrogate not followed by a low surrogate"
				high))))
       ((%low-surrogate? (car rest))
	(%surrogate-error "low surrogate not preceded by a high surrogate"
			  (car rest)))
       (else
	(loop (cdr rest) (cons (integer->char (car rest)) acc))))))
  (define (token str . comp?)
    (let ((cmp? (if (null? comp?)
		    char=?
		    comp?)))
     (lambda (starting-results)
       (let loop ((pos 0) (results starting-results))
	 (if (= pos (string-length str))
	     (make-result str results)
	     (let ((res-token-value (parse-results-token-value results)))
	       (if (and res-token-value (cmp? res-token-value (string-ref str pos)))
		   (loop (+ pos 1) (parse-results-next results))
		   (make-expected-result (parse-results-position starting-results) str)))))))))
