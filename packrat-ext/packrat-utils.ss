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
	  jstring-body
	  token
	  )
  (import (ext packrat)
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

  (define jstring-body
    (packrat-parser any
		    (any ((c <- jstring-char s <- any) (cons c s))
			 ((c <- jstring-char) (cons c '())))
		    (jstring-char (((! '#\\) (! '#\") c <- (? true)) c)
				  (('#\\ '#\n) #\newline)
				  (('#\\ '#\b) #\backspace)
				  (('#\\ '#\f) #\page)
				  (('#\\ '#\r) #\return)
				  (('#\\ '#\t) #\tab)
				  (('#\\ '#\u a <- (? char-valid-hex?) b <- (? char-valid-hex?) c <- (? char-valid-hex?) d <- (? char-valid-hex?) )
				   (integer->char (string->number (list->string (list a b c d)) 16)))
				  (('#\\ (! '#\u) c <- (? true)) c))))
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
