;; JSON Parser Test Cases for packrat-extended
;; Tests for (chaos json) library: json-read, json-write, json-null?

(import (rnrs)
        (chaos json))

;; ---- Test infrastructure ----

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (assert-equal label actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
        (set! pass-count (+ pass-count 1))
        (display "  PASS: ")
        (display label)
        (newline))
      (begin
        (set! fail-count (+ fail-count 1))
        (display "  FAIL: ")
        (display label)
        (newline)
        (display "    expected: ")
        (write expected)
        (newline)
        (display "    actual:   ")
        (write actual)
        (newline))))

(define (assert-true label actual)
  (assert-equal label actual #t))

(define (json-parse str)
  (json-read (open-string-input-port str)))

(define (json-roundtrip str)
  (let ((parsed (json-parse str)))
    (let-values (((port extract) (open-string-output-port)))
      (json-write parsed port)
      (extract))))

(define (json-parse-fails? str)
  (guard (exn [#t #t])
    (json-parse str)
    #f))

(define (section title)
  (newline)
  (display "=== ")
  (display title)
  (display " ===")
  (newline))

;; ---- 1. Basic value parsing ----

(section "Boolean parsing")
(assert-equal "true" (json-parse "true") #t)
(assert-equal "false" (json-parse "false") #f)

(section "Null parsing")
(assert-true "null is json-null" (json-null? (json-parse "null")))

(section "Integer parsing")
(assert-equal "integer 42" (json-parse "42") 42)
(assert-equal "integer 0" (json-parse "0") 0)
(assert-equal "integer 1" (json-parse "1") 1)
(assert-equal "integer 123456" (json-parse "123456") 123456)

(section "Float parsing")
(assert-equal "float 3.14" (json-parse "3.14") 3.14)
(assert-equal "float 0.5" (json-parse "0.5") 0.5)
(assert-equal "float .5" (json-parse ".5") 0.5)
(assert-equal "float 1." (json-parse "1.") 1.0)

(section "Scientific notation")
(assert-equal "1e2" (json-parse "1e2") 100.0)
(assert-equal "1E2" (json-parse "1E2") 100.0)
(assert-equal "25e1" (json-parse "25e1") 250.0)
(assert-equal "3.14e0" (json-parse "3.14e0") 3.14)

;; ---- 2. String parsing ----

(section "Basic strings")
(assert-equal "simple string" (json-parse "\"hello\"") "hello")
;; Note: empty string "" is not supported by the current jstring-body grammar
;; (it requires at least one character)
(assert-equal "string with spaces" (json-parse "\"hello world\"") "hello world")

(section "String escape sequences")
(assert-equal "escaped quote" (json-parse "\"a\\\"b\"") "a\"b")
(assert-equal "escaped backslash" (json-parse "\"a\\\\b\"") "a\\b")
(assert-equal "escaped slash" (json-parse "\"a\\/b\"") "a/b")
(assert-equal "escaped newline" (json-parse "\"a\\nb\"") "a\nb")
(assert-equal "escaped tab" (json-parse "\"a\\tb\"") "a\tb")
(assert-equal "escaped backspace" (json-parse "\"a\\bb\"") "a\x8;b")
(assert-equal "escaped formfeed" (json-parse "\"a\\fb\"") "a\xC;b")
(assert-equal "escaped carriage return" (json-parse "\"a\\rb\"") "a\rb")

(section "Unicode escapes")
(assert-equal "unicode A" (json-parse "\"\\u0041\"") "A")
(assert-equal "unicode euro" (json-parse "\"\\u20AC\"") "\x20AC;")

;; ---- 3. Array parsing ----

(section "Array parsing")
(assert-equal "empty array" (json-parse "[]") '())
(assert-equal "single element array" (json-parse "[1]") '(1))
(assert-equal "multi element array" (json-parse "[1, 2, 3]") '(1 2 3))
(assert-equal "nested arrays" (json-parse "[[1, 2], [3, 4]]") '((1 2) (3 4)))
(assert-equal "mixed type array"
  (let ((result (json-parse "[1, \"two\", true]")))
    (list (car result) (cadr result) (caddr result)))
  '(1 "two" #t))

;; Test array with null
(let ((result (json-parse "[1, null, 3]")))
  (assert-equal "array with null - first" (car result) 1)
  (assert-true "array with null - middle is null" (json-null? (cadr result)))
  (assert-equal "array with null - last" (caddr result) 3))

;; ---- 4. Object parsing ----

(section "Object parsing")
(assert-equal "empty object" (json-parse "{}") '#())
(assert-equal "single key object" (json-parse "{\"a\": 1}") '#(("a" . 1)))
(assert-equal "multi key object"
  (json-parse "{\"a\": 1, \"b\": 2}")
  '#(("a" . 1) ("b" . 2)))

;; Nested object
(let ((result (json-parse "{\"outer\": {\"inner\": 42}}")))
  (assert-equal "nested object outer key" (car (vector-ref result 0)) "outer")
  (assert-equal "nested object inner value"
    (cdr (vector-ref (cdr (vector-ref result 0)) 0)) 42))

;; ---- 5. Mixed nesting ----

(section "Mixed nesting")
(let ((result (json-parse "{\"nums\": [1, 2, 3]}")))
  (assert-equal "object with array - key" (car (vector-ref result 0)) "nums")
  (assert-equal "object with array - value" (cdr (vector-ref result 0)) '(1 2 3)))

(let ((result (json-parse "[{\"a\": 1}, {\"b\": 2}]")))
  (assert-equal "array of objects - first" (vector-ref (car result) 0) '("a" . 1))
  (assert-equal "array of objects - second" (vector-ref (cadr result) 0) '("b" . 2)))

;; ---- 6. Whitespace handling ----

(section "Whitespace handling")
(assert-equal "leading whitespace" (json-parse "  42") 42)
(assert-equal "trailing whitespace in array" (json-parse "[ 1 , 2 , 3 ]") '(1 2 3))
(assert-equal "newlines in object"
  (json-parse "{\n  \"a\": 1,\n  \"b\": 2\n}")
  '#(("a" . 1) ("b" . 2)))
(assert-equal "tabs in array" (json-parse "[\t1,\t2\t]") '(1 2))

;; ---- 7. Comment handling ----

(section "Comment handling")
(assert-equal "c-style comment" (json-parse "/* comment */ 42") 42)
(assert-equal "line comment" (json-parse "// comment\n42") 42)
(assert-equal "comment in array" (json-parse "[1, /* skip */ 2]") '(1 2))
(assert-equal "comment in object"
  (json-parse "{\"a\": 1, // line comment\n\"b\": 2}")
  '#(("a" . 1) ("b" . 2)))

;; ---- 8. Round-trip tests (json-read + json-write) ----

(section "Round-trip tests (json-write)")
(assert-equal "write integer" (json-roundtrip "42") "42")
(assert-equal "write string" (json-roundtrip "\"hello\"") "\"hello\"")
(assert-equal "write true" (json-roundtrip "true") "true")
(assert-equal "write false" (json-roundtrip "false") "false")
(assert-equal "write null" (json-roundtrip "null") "null")
(assert-equal "write empty array" (json-roundtrip "[]") "[]")
(assert-equal "write array" (json-roundtrip "[1, 2, 3]") "[1, 2, 3]")
(assert-equal "write empty object" (json-roundtrip "{}") "{}")
(assert-equal "write object" (json-roundtrip "{\"a\": 1}") "{\"a\": 1}")

;; ---- 9. Negative numbers (known limitation) ----
;; Note: the jnumber grammar does not handle a leading minus sign,
;; so negative numbers are not supported by the parser.

(section "Negative numbers (known limitation)")
(assert-true "negative integer fails" (json-parse-fails? "-1"))
(assert-true "negative float fails" (json-parse-fails? "-3.14"))

;; ---- 10. Float round-trip ----

(section "Float round-trip")
(assert-equal "roundtrip float 3.14" (json-roundtrip "3.14") "3.14")
(assert-equal "roundtrip float 0.5" (json-roundtrip "0.5") "0.5")
(assert-equal "roundtrip float 1.0" (json-roundtrip "1.") "1.0")

;; ---- 11. Malformed input ----

(section "Malformed input")
(assert-true "unclosed string" (json-parse-fails? "\"hello"))
(assert-true "unclosed array" (json-parse-fails? "[1, 2"))
(assert-true "unclosed object" (json-parse-fails? "{\"a\": 1"))
(assert-true "trailing comma in array" (json-parse-fails? "[1, 2,]"))
(assert-true "trailing comma in object" (json-parse-fails? "{\"a\": 1,}"))
(assert-true "bare word" (json-parse-fails? "undefined"))

;; ---- 12. Edge cases ----

(section "Edge cases")
;; Deeply nested
(assert-equal "deeply nested arrays" (json-parse "[[[1]]]") '(((1))))
(assert-equal "deeply nested objects"
  (let ((result (json-parse "{\"a\": {\"b\": {\"c\": 1}}}")))
    (cdr (vector-ref (cdr (vector-ref (cdr (vector-ref result 0)) 0)) 0)))
  1)

;; Long string
(assert-equal "long string"
  (string-length (json-parse "\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\""))
  52)

;; Duplicate keys (document behavior - both entries kept)
(let ((result (json-parse "{\"a\": 1, \"a\": 2}")))
  (assert-equal "duplicate keys - count" (vector-length result) 2))

;; ---- Summary ----

(newline)
(display "========================================")
(newline)
(display "Results: ")
(display pass-count)
(display " passed, ")
(display fail-count)
(display " failed, ")
(display test-count)
(display " total")
(newline)
(display "========================================")
(newline)

(when (> fail-count 0)
  (exit 1))
