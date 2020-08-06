;; from chicken's egg wiki.call-cc.org/eggref/4/json
(library (chaos json-ruby)
  (export packrat-reader
	  json-ruby-parser)
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


  (define (hashtable->vector ht)
    (let-values (((keys values) (hashtable-entries ht)))
      (vector-map cons keys values)))

  (define packrat-reader
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
			(values pos #f))
		      (let ((old-pos pos))
			(set! pos (update-parse-position pos x))
			(values old-pos (cons x x)))))))))

      

      

      (define (read-any parser p)
	(let ((result (parser (base-generator->results (generator p)))))
	  (if (parse-result-successful? result)
	      (parse-result-semantic-value result)
	      (error 'json-read "JSON Parse Error"
		     (let ((e (parse-result-error result)))
		       (list 'json-parse-error
			     (parse-position->string (parse-error-position e))
			     (parse-error-expected e)
			     (parse-error-messages e)))))))

      (lambda (parser . maybe-port)
	(read-any parser (if (pair? maybe-port) (car maybe-port) (current-input-port))))))
  (define char-valid-ruby-symbol?
    (test-any
     char-alphabetic?
     char-numeric?
     (lambda (x)
       (eq? x #\:))
     (lambda (x)
       (eq? 'Pc
		(char-general-category x)))))
  (define json-ruby-parser
    (packrat-parser
     any
     (any ((white '#\{ entries <- table-entries white '#\}) (list->vector entries))
	  ((white '#\# '#\< s <- ruby-symbol white entries <- obj-entries white '#\>) (cons s (list->vector entries)))
	  ((white '#\[ entries <- array-entries white '#\]) entries)
	  ((s <- jstring) s)
	  ((s <- ruby-symbol) s)
	  ((n <- jnumber) n)
	  ((white (token "true")) #t)
	  ((white (token "false")) #f)
	  ((white (token "nil")) '())
	  ((white (token "null")) (void)))
     (white-space ((a <- (? char-whitespace?) white-space) 'whitespace)
		  ((b <- comment) 'whitespace)
		  )
     (white  ((white-space) 'whitespace) 
	     )

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
     (table-entry ((key <- jstring white '#\= '#\> val <- any) (cons key val))
		  ((key <- ruby-symbol white '#\= '#\> val <- any) (cons key val))
		  )
     (obj-entries ((a <- obj-entries-nonempty) a)
		  (() '()))
     (obj-entries-nonempty ((entry <- obj-entry (? char-whitespace?) white entries <- obj-entries-nonempty) (cons entry entries))
			   ((entry <- obj-entry) (list entry)))
     (obj-entry 
      ((key <- ruby-symbol white '#\= val <- any) (cons key val))
      )
     (array-entries ((a <- array-entries-nonempty) a)
		    (() '()))
     (array-entries-nonempty ((entry <- any white '#\, entries <- array-entries-nonempty) (cons entry entries))
			     ((entry <- any) (list entry)))

     
     (jstring ((white '#\" body <- jstring-body '#\") (list->string body))
	      ((white '#\" '#\") ""))
     (ruby-symbol ((white (/ ('#\:) ('#\@))
			  s <- symbol-body)
		   (string->symbol (list->string s)))
		  ((white c <- (? char-upper-case?) s <- symbol-body)
		   (string->symbol (list->string (cons c s)))))
     (symbol-body ((c <- (? char-valid-ruby-symbol?)
		      rest <- symbol-body)
		   (cons c rest))
		  ((c <- (? char-valid-ruby-symbol?)) (cons c '())))
     (jnumber
      ((white body <- jfixpoint '#\e e <- jinteger) (* (expt 10 (car e)) body))
      ((white body <- jinteger '#\e e <- jinteger) (* (expt 10 (car e)) 1.0 (car body)))
      ((white body <- jfixpoint) body)
      ((white body <- jinteger) (car body)))
     (jinteger ((di <- (? char-numeric?) dr <- jinteger) (cons
							  (+ (car dr) (* (expt 10 (+ 1 (cdr dr))) (- (char->integer di) 48) ))
							  (+ 1 (cdr dr))))
	       ((di <- (? char-numeric?)) (cons (- (char->integer di) 48) 0)) ;; 48 is (char->integer #\0)
	       )
     (jfixpoint ((a <- jinteger '#\. b <- jinteger) (+ (car a) (/ (car b) (expt 10 (+ 1.0 (cdr b))))))
		(('#\. b <- jinteger) (/ (car b) (expt 10 (+ 1.0 (cdr b)))))
		((b <- jinteger '#\.) (* 1.0 (car b)))
		)))

  )
