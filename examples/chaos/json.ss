;; from chicken's egg wiki.call-cc.org/eggref/4/json
;; From https://github.com/ktakashi/json-tools
(library (chaos json)
  (export json-write
	  json-read
	  json-read-document
	  make-json-null
	  json-null?)
  (import (rnrs)
	  (rnrs r5rs)
	  (packrat-ext packrat)
	  (packrat-ext packrat-utils))

  (define (void) 'null)

  ;; JSON implementation for Scheme
  ;; See http://www.json.org/ or http://www.crockford.com/JSON/index.html
  ;;
  ;; Copyright (c) 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
  ;; Copyright (c) 2005 LShift Ltd. <query@lshift.net>
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

  ;; JSON Structures are represented as vectors: #((symbol . value) (symbol . value) ...)
  ;; JSON Arrays are lists
  ;;
  ;; Non-standard extensions (not valid JSON per RFC 8259, kept intentionally):
  ;;   - Comments: /* ... */ block comments and // ... line comments are supported
  ;;   - Leading decimal point: .5 parses as 0.5
  ;;   - Trailing decimal point: 1. parses as 1.0
  ;;


  (define (hashtable->vector ht)
    (let-values (((keys values) (hashtable-entries ht)))
      (vector-map cons keys values)))

  (define-record-type json-null)

  (define json-write
    (let ()
      ;; JSON has no syntax for Scheme's \xNN; escape, so a string cannot go
      ;; through R6RS `write`: a control character comes back out as "\x1;",
      ;; which json-read then rejects. RFC 8259 s7 requires U+0000-U+001F,
      ;; the quote and the backslash to be escaped, and permits everything
      ;; else raw.
      (define (write-json-string s p)
	(display "\"" p)
	(string-for-each
	 (lambda (c)
	   (let ((n (char->integer c)))
	     (cond
	      ((char=? c #\") (display "\\\"" p))
	      ((char=? c #\\) (display "\\\\" p))
	      ((= n 8) (display "\\b" p))
	      ((= n 9) (display "\\t" p))
	      ((= n 10) (display "\\n" p))
	      ((= n 12) (display "\\f" p))
	      ((= n 13) (display "\\r" p))
	      ((< n #x20)
	       (display "\\u" p)
	       (let ((hex (string-downcase (number->string n 16))))
		 (display (make-string (- 4 (string-length hex)) #\0) p)
		 (display hex p)))
	      (else (write-char c p)))))
	 s)
	(display "\"" p))

      ;; Chez prints a subnormal flonum with its precision attached, as in
      ;; 1e-308|51, and that bar is not JSON. The digits before it are the
      ;; shortest that read back as the same double, so drop the annotation.
      ;; No other number printed here can contain a bar.
      (define (number->json-string x)
	(let* ((s (number->string x))
	       (n (string-length s)))
	  (let loop ((i 0))
	    (cond
	     ((= i n) s)
	     ((char=? (string-ref s i) #\|) (substring s 0 i))
	     (else (loop (+ i 1)))))))

      ;; A JSON number is a decimal literal, so an exact rational such as 1/3,
      ;; a non-finite flonum such as +inf.0 and a complex number have no JSON
      ;; syntax at all. R6RS `write` prints Scheme syntax for each, which
      ;; json-read cannot read back, so raise instead of emitting it.
      (define (write-number x p)
	(if (or (not (real? x))
		(and (exact? x) (not (integer? x)))
		(and (inexact? x) (not (finite? x))))
	    (error 'json-write "Number has no JSON representation in json-write" x)
	    (display (number->json-string x) p)))

      (define (write-ht vec p)
	(display "{" p)
	(do ((need-comma #f #t)
	     (i 0 (+ i 1)))
	    ((= i (vector-length vec)))
	  (if need-comma
	      (display ", " p)
	      (set! need-comma #t))
	  (let* ((entry (vector-ref vec i))
		 (k (car entry))
		 (v (cdr entry)))
	    (cond
	     ((symbol? k) (write-json-string (symbol->string k) p))
	     ((string? k) (write-json-string k p)) ;; for convenience
	     (else (error 'json-write "Invalid JSON table key in json-write" k)))
	    (display ": " p)
	    (write-any v p)))
	(display "}" p))

      (define (write-array a p)
	(display "[" p)
	(let ((need-comma #f))
	  (for-each (lambda (v)
		      (if need-comma
			  (display ", " p)
			  (set! need-comma #t))
		      (write-any v p))
		    a))
	(display "]" p))

      (define (write-any x p)
	(cond
	 ((hashtable? x) (write-ht (hashtable->vector x) p))
	 ((vector? x) (write-ht x p))
	 ;;((pair? x) (write-array x p))
	 ((list? x) (write-array x p))
	 ((symbol? x) (write-json-string (symbol->string x) p)) ;; for convenience
	 ((string? x) (write-json-string x p))
	 ((number? x) (write-number x p))
	 ((boolean? x) (display (if x "true" "false") p))
	 ((json-null? x) (display "null" p))
	 (else (error 'json-write "Invalid JSON object in json-write" x))))

      (lambda (x . maybe-port)
	(write-any x (if (pair? maybe-port) (car maybe-port) (current-output-port))))))

  ;; json-read and json-read-document share one grammar and one generator, so
  ;; they are built together here and pulled apart below. packrat-parser's
  ;; first argument is the start nonterminal, and the macro splices it in as
  ;; the body of the `let` it generates, so naming a pair of nonterminals there
  ;; is how one grammar serves two entry points.
  (define json-readers
    (let ()
      (define (generator p)
	(let ((ateof #f)
	      (pos (top-parse-position "<?>")))
	  (lambda ()
	    (if ateof
		(values pos #f)
		(let ((x (read-char p)))
		  (if (eof-object? x)
		      (begin
			(set! ateof #t)
			(values pos (cons #\x04 #\x04))
			)
		      (let ((old-pos pos))
			(set! pos (update-parse-position pos x))
			(values old-pos (cons x x)))))))))
      (define parsers
	(packrat-parser
			(cons any document)
			;; A JSON text is one value and nothing else (RFC 8259
			;; s2). `any` stops as soon as that value is complete
			;; and never looks at the rest of the stream, which is
			;; what json-read wants: it takes a port, so a caller
			;; may read successive values out of one stream.
			;; `document` is the same value followed by end of
			;; input, for a caller reading a whole document.
			(document ((v <- any white '#\x04) v))
			(any ((white '#\{ entries <- table-entries white '#\}) (list->vector entries))
			     ((white '#\[ entries <- array-entries white '#\]) entries)
			     ((s <- jstring) s)
			     ((n <- jnumber) n)
			     ((white (token "true") (! (? json-token-char?))) #t)
			     ((white (token "false") (! (? json-token-char?))) #f)
			     ((white (token "null") (! (? json-token-char?))) (make-json-null))
			     ((white '#\x04) (eof-object)))
			(white ((a <- (? char-whitespace?) white) 'whitespace)
			       ((b <- (? char-whitespace?)) 'whitespace)
			       ((b <- comment) 'whitespace)
			       )

			;; Non-standard extension: C-style and line comments
			(comment (((token "/*") b <- comment-body) b)
				 (((token "//") b <- skip-to-newline) b)
				 (() 'whitespace))
			(comment-body (((token "*/") w <- white) w)
				      (((? true) comment-body) 'skipped-comment-char))
			(skip-to-newline (((? (inverse char-newline?))
					   skip-to-newline) 'whitespace)
					 (((? char-newline?) white) 'whitespace)
					 )
			
			(table-entries ((a <- table-entries-nonempty) a)
				       (() '()))
			(table-entries-nonempty ((entry <- table-entry white '#\, entries <- table-entries-nonempty) (cons entry entries))
						((entry <- table-entry) (list entry)))
			(table-entry ((key <- jstring white '#\: val <- any) (cons key val)))
			(array-entries ((a <- array-entries-nonempty) a)
				       (() '()))
			(array-entries-nonempty ((entry <- any white '#\, entries <- array-entries-nonempty) (cons entry entries))
						((entry <- any) (list entry)))
			(jstring ((white '#\" body <- jstring-body '#\") (jstring-body->string body)))
			;; A number is delimited the same way a literal is: the
			;; longest alternative that matches wins, and whatever
			;; it leaves behind must not be another token character.
			;; One lookahead here covers all eight alternatives,
			;; and because ordered choice cannot backtrack into
			;; jnumber-token once it has succeeded, `0x10` fails
			;; outright rather than falling back to a shorter match.
			(jnumber ((n <- jnumber-token (! (? json-token-char?))) n))
			(jnumber-token
			 ((white '#\- body <- jfixpoint (/ ('#\E) ('#\e)) e <- jexponent) (- (* (expt 10 e) 1.0 body)))
			 ((white '#\- body <- jsafeint (/ ('#\E) ('#\e)) e <- jexponent) (- (* (expt 10 e) 1.0 (car body))))
			 ((white '#\- body <- jfixpoint) (- body))
			 ((white '#\- body <- jsafeint) (- (car body)))
			 ((white  body <- jfixpoint (/ ('#\E) ('#\e)) e <- jexponent) (* (expt 10 e) 1.0 body))
			 ((white  body <- jsafeint (/ ('#\E) ('#\e)) e <- jexponent) (* (expt 10 e) 1.0 (car body)))
			 ((white body <- jfixpoint) body)
			 ((white body <- jsafeint) (car body)))
			;; jexponent: exponent part after e/E, supports optional +/-
			(jexponent (('#\+ e <- jinteger) (car e))
				   (('#\- e <- jinteger) (- (car e)))
				   ((e <- jinteger) (car e)))
			;; jsafeint: integer that rejects leading zeros (RFC 8259)
			(jsafeint (('#\0 (! (? char-numeric?))) '(0 . 0))
				  ((di <- (? char-nonzero-digit?) dr <- jinteger) (cons
									  (+ (car dr) (* (expt 10 (+ 1 (cdr dr))) (- (char->integer di) 48) ))
									  (+ 1 (cdr dr))))
				  ((di <- (? char-nonzero-digit?)) (cons (- (char->integer di) 48) 0)))
			(jinteger ((di <- (? char-numeric?) dr <- jinteger) (cons
									  (+ (car dr) (* (expt 10 (+ 1 (cdr dr))) (- (char->integer di) 48) ))
									  (+ 1 (cdr dr))))
				  ((di <- (? char-numeric?)) (cons (- (char->integer di) 48) 0)) ;; 48 is (char->integer #\0)
				  )
			;; Non-standard extensions: leading/trailing decimal point (.5, 1.)
			;; These are intentional lenient parsing features, not valid JSON per RFC 8259.
			(jfixpoint ((a <- jsafeint '#\. b <- jinteger) (+ (car a) (/ (car b) (expt 10 (+ 1.0 (cdr b))))))
				   (('#\. b <- jinteger) (/ (car b) (expt 10 (+ 1.0 (cdr b)))))
				   ((b <- jsafeint '#\.) (exact->inexact (car b))))))

      (define (read-with parse p)
	(let ((result (parse (base-generator->results (generator p)))))
	  (if (parse-result-successful? result)
	      (parse-result-semantic-value result)
	      (error 'json-read "JSON Parse Error"
		     (let ((e (parse-result-error result)))
		       (list 'json-parse-error
			     (parse-position->string (parse-error-position e))
			     (parse-error-expected e)
			     (parse-error-messages e)))))))

      (define (entry-point parse)
	(lambda maybe-port
	  (read-with parse
		     (if (pair? maybe-port) (car maybe-port) (current-input-port)))))

      (cons (entry-point (car parsers))
	    (entry-point (cdr parsers)))))

  (define json-read (car json-readers))

  ;; Reads one JSON value and then requires end of input, so a document with
  ;; anything after its first complete value is a parse error rather than a
  ;; silently truncated read. Trailing whitespace and trailing comments are
  ;; still allowed. Empty input is an error here, where json-read returns the
  ;; end-of-file object.
  (define json-read-document (cdr json-readers))

  )
