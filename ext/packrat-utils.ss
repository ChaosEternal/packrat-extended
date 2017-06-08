(library (ext packrat-utils)
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
	     (if (cmp? (parse-results-token-value results) (string-ref str pos))
		 (loop (+ pos 1) (parse-results-next results))
		 (make-expected-result (parse-results-position starting-results) str))))))))
