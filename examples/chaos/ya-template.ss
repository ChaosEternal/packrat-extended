;; Packrat Parser Library Extended
;;
;; Copyright (c) 2020 Google LLC.
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

(library (chaos ya-template)
  (export tpl-pass-0
	  tpl-pass-1
	  apply-parsed-template
	  apply-template)
  (import (rnrs r5rs)
	  (rnrs bytevectors)
	  (packrat-ext packrat)
	  (packrat-ext packrat-utils)
	  (guile)
	  (system base compile)
)

  (define (pos->source-properties pos)
    (list (cons 'finename  (parse-position-file pos))
	  (cons 'line  (parse-position-line pos))
	  (cons 'column (parse-position-column pos))))

  (define (set-obj-pos! obj pos)
    (let ((prt (pos->source-properties pos)))
      (begin
	(set-source-properties! obj pos)
	obj)))

  (define (tpl-pass-0 prt)
    (define (generator p)
      (let ((ateof #f)
	    (pos (top-parse-position (format #f "~a" p))))
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

    (define (wrap-guile-reader starting-results)
      (let* ((current-results starting-results)
	     (init-position (parse-results-position starting-results))
	     (reader! (lambda ()
			(let
			    ((cur-value (parse-results-token-value current-results)))
			  (if (eq? cur-value #\x04)
			      #f
			      (begin
				(set! current-results (parse-results-next current-results))
				cur-value)))))
	     (rport (make-soft-port (vector
				     #f #f #f reader! #f #f)
				    "ro"))
	     (_ (set-port-filename! rport (parse-position-file init-position)))
	     (_ (set-port-line! rport (- (parse-position-line init-position) 1)))
	     (_ (set-port-column! rport (+ (parse-position-column init-position) 1)))
	     (_ (setvbuf rport 'none 0)))
	(make-result (read rport) current-results
		     )))
    
    (define parser
      (packrat-parser any
		      (any ((strs <- strings ) strs))
		      (string (( (token "{%")) "")
			      ((c <- (? (lambda x #t)) str <- string ) (format #f "~a~a" c str)))
		      (ws (((? char-whitespace?) ws) 'ws)
			  (() 'ws))
		      (end-token ((ws (token "%}")) 'tkn)
				 ((ws (token "-%}") skip-to-newline) 'tkn)
					; (((token "%}")) 'tkn)
				 )
		      (skip-to-newline (((? (inverse char-newline?))
					 skip-to-newline) 'whitespace)
				       (((? char-newline?)) 'whitespace))
		      (begin-token (((? char-whitespace?)) 'eval)
				   (((token "foreach")) 'foreach)
				   (((token "if")) 'if)
				   (((token "elif")) 'elif)
				   (((token "=")) 'value))
		      (sexp ((pos <- ^
				  btag <- begin-token gsexp <- wrap-guile-reader end-token
				  ) `((,btag . ,pos) . ,gsexp))
			    ((pos <- ^ (token "end") end-token) `((end . ,pos)))
			    )
		      (end-string 
		       (((! string) x <- (? (lambda x #t)) estr <- end-string (! (token "{%")) ) (format #f "~a~a" x estr))
		       (('#\x04 ) "")
		       )
		      (strings 
		       ((pos <- ^ str <- string s <- sexp strs <- strings) `(((text . ,pos) . ,(set-obj-pos! str pos)) ,s ,@strs))
		       ((pos <- ^ estr <- end-string) `(((text . ,pos) . ,(set-obj-pos! estr pos)))))
		      
		      ))

    (define (read-any p)
      (let ((result (parser (base-generator->results (generator p)))))
	(if (parse-result-successful? result)
	    (parse-result-semantic-value result)
	    (error 'my-tpl "parse error"
		   (let ((e (parse-result-error result)))
		     (list 'tpl-pass-0
			   (parse-position->string (parse-error-position e))
			   (parse-error-expected e)
			   (parse-error-messages e)))))))
    (read-any prt))

  (define (tpl-pass-1 token-list)
    (define (generator token-list)
      (let ((stream token-list))
	(lambda ()
	  (if (null? stream)
	      (values #f #f)
	      (let ((base-token (car stream))
		    (tag (caar (car stream)))
		    (pos (cdar (car stream)))
		    (value (cdr (car stream))))
		(set! stream (cdr stream))
		(values pos (cons tag value)))))))


    (define parser
      (packrat-parser any
		      (any ((ae <- any-expr '#f) (cons-source ae 'begin ae)))
		      (any-expr
		       ((txt <- 'text rest <- any-expr) (cons-source txt `(display ,txt) rest))
		       ((value <- 'value rest <- any-expr) (cons-source value
									(cons-source value
										     'display (cons-source value value '())) rest))
		       ((eval <- 'eval rest <- any-expr) (cons-source eval eval rest))
		       ((fe <- foreach rest <- any-expr ) (cons-source fe fe rest))
		       ((iflist <- iflist rest <- any-expr ) (cons-source iflist iflist rest))
		       (() '()))
		      (foreach ((forin <- 'foreach infor <- any-expr 'end)
				`(for-each
				  (lambda ,(car forin)
				    (begin ,@infor)
				    )
				  ,@(cdr forin))))
		      (iflist ((iflist <- 'if ifbody <- any-expr elif <- elif )
			       `(cond (,iflist (begin ,@ifbody))
				      ,@elif)))
		      (elif   ((elif-list <- 'elif ifbody <- any-expr elif-rest <- elif)
			       `((,elif-list (begin ,@ifbody))
				 ,@elif-rest))
			      (('end) '()))))
    (define (read-any p)
      (let ((result (parser (base-generator->results (generator p)))))
	(if (parse-result-successful? result)
	    (parse-result-semantic-value result)
	    (error 'tpl-pass-1 "parse error"
		   (let ((e (parse-result-error result)))
		     (list 'tpl-pass-1
			   (parse-position->string (parse-error-position e))
			   (parse-error-expected e)
			   (parse-error-messages e)))))))
    (read-any token-list)
    )

  (define (apply-parsed-template tmpl value-dict)
    (let* ((the-module (resolve-module `(,(gensym "temp-env"))))
	   (_ (module-use! the-module (resolve-module '(guile))))
	   (_ (module-use! the-module (resolve-module '(ice-9 ports))))
	   (_ (for-each (lambda (x)
			  (module-define! the-module
					  (car x)
					  (cdr x)))
			value-dict))
	   (prc (compile tmpl #:env the-module)))
      prc))

  (define (apply-template tmpl-port value-dict)
    (let* ((pass-0-result (tpl-pass-0 tmpl-port))
	   (pass-1-result (tpl-pass-1 pass-0-result)))
      (apply-parsed-template pass-1-result value-dict)))

  ;; (let ((pass-0-res (tpl-pass-0 (open-input-file (cadr (command-line))))))
  ;;  (pretty-print pass-0-res)
  ;;  (pretty-print (tpl-pass-1 pass-0-res))
  ;;  (apply-parsed-template (tpl-pass-1 pass-0-res)
  ;; 			`((scheme-value . "some-scheme-value")
  ;; 			  (scheme-expression . ,(lambda () (/ 1 0)))
  ;; 			  (list1 . (a b c))
  ;; 			  (list2 . (1 2 3))
  ;; 			  (predict-cond1? . #t)
  ;; 			  (another-expression . ,(lambda () (+ 1 3))))))

  )
