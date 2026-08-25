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

(define (json-write-to-string x)
  (let-values (((port extract) (open-string-output-port)))
    (json-write x port)
    (extract)))

(define (json-roundtrip str)
  (json-write-to-string (json-parse str)))

(define (json-parse-fails? str)
  (guard (exn [#t #t])
    (json-parse str)
    #f))

;; Stricter than json-parse-fails?: the parser must reject the input itself,
;; raising the json-read parse error, rather than dying inside a primitive
;; such as integer->char. json-parse-fails? cannot tell those apart.
(define (json-parse-fails-as-json-read? str)
  (guard (exn [(and (who-condition? exn) (eq? (condition-who exn) 'json-read)) #t]
              [#t #f])
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

(section "Surrogate pair escapes")
;; RFC 8259 section 7: characters outside the BMP are escaped as a UTF-16
;; surrogate pair, and each half on its own is not a Unicode scalar value.
(assert-equal "surrogate pair G-clef" (json-parse "\"\\uD834\\uDD1E\"") "\x1D11E;")
(assert-equal "surrogate pair in context" (json-parse "\"a\\uD834\\uDD1Eb\"") "a\x1D11E;b")
(assert-equal "two surrogate pairs" (json-parse "\"\\uD834\\uDD1E\\uD83D\\uDE00\"") "\x1D11E;\x1F600;")
(assert-equal "pair adjacent to BMP escape" (json-parse "\"\\u0041\\uD834\\uDD1E\"") "A\x1D11E;")
(assert-true "lone high surrogate rejected"
             (json-parse-fails-as-json-read? "\"\\uD834\""))
(assert-true "lone low surrogate rejected"
             (json-parse-fails-as-json-read? "\"\\uDD1E\""))
(assert-true "high surrogate then BMP escape rejected"
             (json-parse-fails-as-json-read? "\"\\uD834\\u0041\""))
(assert-true "high surrogate then plain char rejected"
             (json-parse-fails-as-json-read? "\"\\uD834x\""))
(assert-true "low surrogate then high surrogate rejected"
             (json-parse-fails-as-json-read? "\"\\uDD1E\\uD834\""))

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

;; ---- 9. Negative numbers ----

(section "Negative numbers")
(assert-equal "negative integer" (json-parse "-1") -1)
(assert-equal "negative float" (json-parse "-3.14") -3.14)
(assert-equal "negative zero" (json-parse "-0") 0)
(assert-equal "negative scientific" (json-parse "-1e2") -100.0)
(assert-equal "negative float scientific" (json-parse "-3.14e0") -3.14)
(assert-equal "negative in array" (car (json-parse "[-1, 2]")) -1)
(assert-equal "negative in object" (cdr (vector-ref (json-parse "{\"a\": -42}") 0)) -42)

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

;; ---- 12. Empty string support ----

(section "Empty string support")
(assert-equal "empty string" (json-parse "\"\"") "")
(assert-equal "empty string in array" (json-parse "[\"\" , 1]") '("" 1))
(assert-equal "empty string in object" (cdr (vector-ref (json-parse "{\"a\": \"\"}") 0)) "")
(assert-equal "empty string key" (car (vector-ref (json-parse "{\"\": 1}") 0)) "")
(assert-equal "roundtrip empty string" (json-roundtrip "\"\"") "\"\"")

;; ---- 13. Signed exponents ----

(section "Signed exponents")
(assert-equal "1e+2" (json-parse "1e+2") 100.0)
(assert-equal "1E+2" (json-parse "1E+2") 100.0)
(assert-equal "1e-2" (json-parse "1e-2") 0.01)
(assert-equal "1E-2" (json-parse "1E-2") 0.01)
(assert-equal "25e+1" (json-parse "25e+1") 250.0)
(assert-equal "3.14e+0" (json-parse "3.14e+0") 3.14)
;; Use < comparison for float precision: 3.14e-1 may have rounding artifacts
(assert-true "3.14e-1" (< (abs (- (json-parse "3.14e-1") 0.314)) 1e-15))
(assert-equal "-1e+2" (json-parse "-1e+2") -100.0)
(assert-equal "-1e-2" (json-parse "-1e-2") -0.01)

;; ---- 14. Leading zeros rejection ----

(section "Leading zeros rejection")
(assert-equal "single zero" (json-parse "0") 0)
(assert-equal "zero point five" (json-parse "0.5") 0.5)
(assert-true "leading zero 01 rejected" (json-parse-fails? "01"))
(assert-true "leading zero 0123 rejected" (json-parse-fails? "0123"))
(assert-true "leading zero 007 rejected" (json-parse-fails? "007"))
(assert-true "leading zero 00 rejected" (json-parse-fails? "00"))

;; ---- 15. Invalid escape sequence rejection ----

(section "Invalid escape sequence rejection")
(assert-true "invalid escape \\a rejected" (json-parse-fails? "\"\\a\""))
(assert-true "invalid escape \\x rejected" (json-parse-fails? "\"\\x\""))
(assert-true "invalid escape \\1 rejected" (json-parse-fails? "\"\\1\""))
(assert-true "invalid escape \\v rejected" (json-parse-fails? "\"\\v\""))
;; Valid escapes still work
(assert-equal "valid escape \\\"" (json-parse "\"a\\\"b\"") "a\"b")
(assert-equal "valid escape \\\\" (json-parse "\"a\\\\b\"") "a\\b")
(assert-equal "valid escape \\/" (json-parse "\"a\\/b\"") "a/b")

;; ---- 16. json-write emits JSON syntax, not Scheme syntax (PE-103) ----

(section "json-write string escaping")
;; R6RS `write` escapes control characters as Scheme's \x1; which JSON has no
;; syntax for. json-write must emit the JSON escapes instead.
(assert-equal "write control U+0001 as \\u0001"
  (json-roundtrip "\"a\\u0001b\"") "\"a\\u0001b\"")
(assert-equal "write NUL as \\u0000"
  (json-roundtrip "\"\\u0000\"") "\"\\u0000\"")
(assert-equal "write U+001F as \\u001f"
  (json-roundtrip "\"\\u001f\"") "\"\\u001f\"")
(assert-equal "write newline as \\n" (json-roundtrip "\"a\\nb\"") "\"a\\nb\"")
(assert-equal "write tab as \\t" (json-roundtrip "\"a\\tb\"") "\"a\\tb\"")
(assert-equal "write backspace as \\b" (json-roundtrip "\"\\b\"") "\"\\b\"")
(assert-equal "write formfeed as \\f" (json-roundtrip "\"\\f\"") "\"\\f\"")
(assert-equal "write return as \\r" (json-roundtrip "\"\\r\"") "\"\\r\"")
(assert-equal "write quote as \\\"" (json-roundtrip "\"a\\\"b\"") "\"a\\\"b\"")
(assert-equal "write backslash as \\\\" (json-roundtrip "\"a\\\\b\"") "\"a\\\\b\"")
;; Solidus and non-ASCII need no escaping and must survive unescaped.
(assert-equal "write solidus unescaped" (json-roundtrip "\"a\\/b\"") "\"a/b\"")
(assert-equal "write non-ASCII raw" (json-roundtrip "\"\\u00e9\"") "\"\xe9;\"")
;; Object keys go through the same writer.
(assert-equal "write control char in key"
  (json-roundtrip "{\"a\\u0001b\": 1}") "{\"a\\u0001b\": 1}")

(section "json-write output is re-readable")
;; The defect PE-103 names: parse a document the parser accepts, write it, and
;; the result must parse back to the same value.
;; Guarded so a regression here fails its own assertion instead of aborting
;; the run and hiding every later section.
(define (json-reread str)
  (guard (exn [#t 'reread-failed])
    (json-parse (json-roundtrip str))))
(assert-equal "reread control char" (json-reread "\"a\\u0001b\"") "a\x1;b")
(assert-equal "reread NUL" (json-reread "\"\\u0000\"") "\x0;")
(assert-equal "reread newline" (json-reread "\"a\\nb\"") "a\nb")
(assert-equal "reread key with control char"
  (car (vector-ref (json-reread "{\"a\\u0001b\": 1}") 0)) "a\x1;b")

(section "json-write rejects unrepresentable numbers")
(define (json-write-fails-as-json-write? x)
  (guard (exn [(and (who-condition? exn) (eq? (condition-who exn) 'json-write)) #t]
              [#t #f])
    (let-values (((port extract) (open-string-output-port)))
      (json-write x port)
      (extract))
    #f))
(assert-true "exact rational 1/3 rejected" (json-write-fails-as-json-write? 1/3))
(assert-true "exact rational in array rejected"
  (json-write-fails-as-json-write? (list 1/3)))
(assert-true "+inf.0 rejected" (json-write-fails-as-json-write? (/ 1.0 0.0)))
(assert-true "-inf.0 rejected" (json-write-fails-as-json-write? (/ -1.0 0.0)))
(assert-true "+nan.0 rejected" (json-write-fails-as-json-write? (/ 0.0 0.0)))
(assert-true "complex number rejected"
  (json-write-fails-as-json-write? (make-rectangular 1 2)))
;; Representable numbers still write.
(assert-equal "exact integer writes" (json-roundtrip "42") "42")
(assert-equal "negative integer writes" (json-roundtrip "-7") "-7")
(assert-equal "flonum writes" (json-roundtrip "3.14") "3.14")
;; PE-105 (still open) is why this checks the written text rather than the
;; re-read value: "100.0" re-reads as an exact 100, which is a reader defect.
(assert-equal "exponent writes as a JSON decimal" (json-roundtrip "1e2") "100.0")
;; Chez prints a subnormal double with its precision attached, as 1e-308|51.
;; These assert the emitted TEXT, because the bar is a syntax defect that
;; re-reading alone does not always catch: at top level "5e-324|1" parses
;; back to 0.0 without an error, silently corrupting the value.
(assert-equal "subnormal writes without a precision bar"
  (json-roundtrip "1e-308") "1e-308")
;; Fed straight to the writer, because the reader underflows the literal
;; "5e-324" to 0.0 -- a reader-side precision quirk this item does not touch.
(assert-equal "smallest subnormal writes without a precision bar"
  (json-write-to-string 4.9406564584124654e-324) "5e-324")
(assert-equal "subnormal in array writes without a precision bar"
  (json-roundtrip "[1e-320]") "[1e-320]")
(assert-equal "subnormal survives a re-read" (json-reread "[1e-308]") (list 1e-308))
(assert-equal "normal small flonum unaffected" (json-roundtrip "1e-300") "1e-300")

;; ---- 17. Edge cases ----

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

;; ---- Unescaped control characters in strings (PE-101, RFC 8259 section 7) ----
;;
;; The `unescaped` production is %x20-21 / %x23-5B / %x5D-10FFFF, so every
;; character below U+0020 MUST appear as an escape. Quote and backslash are
;; excluded from the range too, and both already have their own escapes.

(define (ctrl n) (integer->char n))

(define (quoted . parts)
  (apply string-append (append (list "\"") parts (list "\""))))

(define (ctrl-string n)
  (quoted "a" (string (ctrl n)) "b"))

(assert-true "raw newline in a string is rejected"
  (json-parse-fails-as-json-read? (ctrl-string 10)))

(assert-true "raw tab in a string is rejected"
  (json-parse-fails-as-json-read? (ctrl-string 9)))

(assert-true "raw carriage return in a string is rejected"
  (json-parse-fails-as-json-read? (ctrl-string 13)))

(assert-true "raw U+0000 in a string is rejected"
  (json-parse-fails-as-json-read? (ctrl-string 0)))

(assert-true "raw U+0001 in a string is rejected"
  (json-parse-fails-as-json-read? (ctrl-string 1)))

(assert-true "raw U+001F in a string is rejected"
  (json-parse-fails-as-json-read? (ctrl-string 31)))

;; Sweep the whole forbidden range rather than sampling it, so a fix that
;; guards only the familiar escapes cannot pass.
(assert-true "every raw character U+0000-U+001F is rejected"
  (let loop ((n 0))
    (cond ((> n 31) #t)
          ((json-parse-fails-as-json-read? (ctrl-string n)) (loop (+ n 1)))
          (else n))))

;; A control character is forbidden wherever a string can appear, including an
;; object member name and a string nested in an array.
(assert-true "raw newline in an object key is rejected"
  (json-parse-fails-as-json-read?
    (string-append "{" (quoted "a" (string (ctrl 10)) "b") ": 1}")))

(assert-true "raw newline in an array element is rejected"
  (json-parse-fails-as-json-read?
    (string-append "[" (ctrl-string 10) "]")))

;; U+0020 is the first allowed character and U+007F is not a JSON control
;; character at all, so neither may be caught by an off-by-one guard.
(assert-equal "space is still allowed raw in a string"
  (json-parse (quoted "a b")) "a b")

(assert-equal "U+007F is still allowed raw in a string"
  (json-parse (quoted (string (ctrl 127))))
  (string (ctrl 127)))

;; The escaped forms of the same characters must keep working.
(assert-equal "escaped newline still parses" (json-parse "\"a\\nb\"") "a\nb")
(assert-equal "escaped tab still parses" (json-parse "\"a\\tb\"") "a\tb")
(assert-equal "escaped \\u0001 still parses"
  (json-parse "\"a\\u0001b\"")
  (string-append "a" (string (ctrl 1)) "b"))

;; A control character outside a string is a separate defect (PE-104) and must
;; not be swept up here: whitespace stays whitespace.
(assert-equal "a newline between tokens is still whitespace"
  (json-parse "[1,\n2]") '(1 2))

;; Rejecting the U+0004 that json-read uses as its end-of-input sentinel must
;; not change how an unterminated string fails.
(assert-true "unterminated string still raises the json-read parse error"
  (json-parse-fails-as-json-read? "\"abc"))

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
