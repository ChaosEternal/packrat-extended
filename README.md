# Packrat Parser Library Extended

An extended packrat parser library for R6RS Scheme, building on the original
[packrat parser](http://wiki.call-cc.org/eggref/4/packrat) by Tony Garnock-Jones
and LShift Ltd.

## Overview

This library provides a packrat parsing framework with memoization and
left-recursion support. It extends the original implementation with:

- **Predicate-based token matching** (`packrat-check-base-pred`) -- match tokens
  by arbitrary predicates, not just equality
- **Position tracking** (`^` operator) -- capture source positions during parsing
- **Inline alternatives** (`/` operator) -- express choice within a sequence
- **Utility combinators** (`packrat-ext/packrat-utils`) -- helpers for character
  classification, string token matching, and JSON string parsing

## Requirements

- [GNU Guile](https://www.gnu.org/software/guile/) (tested with Guile 2.x/3.x)

## Project Structure

```
packrat-ext/
  packrat.ss          Core packrat parser library (R6RS)
  packrat-utils.ss    Utility combinators and helpers
examples/chaos/
  json.ss             JSON reader/writer
  json-ruby.ss        Ruby-style object literal parser
  ya-template.ss      Template engine with embedded Scheme
tests/
  template.scm        Template test driver (no value dict)
  template-test.scm   Template test driver (with value dict)
  test.tpl            Template with foreach/if/elif
  test2.tpl           Template with module import
  tm/simple.scm       Simple test module for template tests
srfi/
  :1/lists.sls        SRFI-1 list library binding
```

## Usage

### Parser DSL

Define grammars with `packrat-parser`:

```scheme
(import (packrat-ext packrat))

(define my-parser
  (packrat-parser expr
    (expr ((a <- term '+ b <- expr) (+ a b))
          ((a <- term) a))
    (term ((a <- factor '* b <- term) (* a b))
          ((a <- factor) a))
    (factor ((a <- 'num) a)
            (('lparen a <- expr 'rparen) a))))
```

Within rules you can:

- `var <- nonterminal` -- bind a nonterminal result
- `var <- 'token` -- match a literal token kind
- `var <- (? predicate)` -- match by predicate
- `var <- ^` -- capture the current source position
- `(/ alt ...)` -- inline alternatives
- `(! pattern ...)` -- negative lookahead

### JSON Parsing

```scheme
(import (chaos json))

(json-read (open-input-string "{\"key\": [1, 2, true]}"))
;; => #(("key" . (1 2 #t)))
```

`json-read` stops as soon as the first complete value has been read and leaves
the rest of the port alone, so successive calls read successive values out of
one stream. Use `json-read-document` when the port is expected to hold one
whole document and nothing else: it reads one value, allows trailing
whitespace and comments, and raises a parse error on anything else.

```scheme
(json-read (open-input-string "[1] trailing junk"))          ;; => (1)
(json-read-document (open-input-string "[1] trailing junk")) ;; => parse error
```

### Template Engine

Templates use `{%` / `%}` delimiters with support for value interpolation,
`foreach` loops, and `if`/`elif` conditionals:

```
Hello {%= name %}!
{%foreach ((item) items) -%}
- {%= item %}
{%end -%}
```

Run a template:

```scheme
(import (chaos ya-template))

(apply-template (open-input-file "template.tpl")
                '((name . "world")
                  (items . (a b c))))
```

## Running Tests

```bash
./run-test.sh
```

This invokes Guile with the appropriate load paths and runs the template tests.

## License

MIT -- see [COPYING](COPYING) for details.

Original packrat parser copyright (c) 2004, 2005 Tony Garnock-Jones and LShift Ltd.
Extensions copyright (c) 2017 Chaos Eternal, (c) 2020 Google LLC.
