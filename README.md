# hazel

A tiny **lazy functional Lisp** in a single C file.

Lisp syntax on the outside, Haskell semantics on the inside: lazy lists, pattern-matching
function clauses, list comprehensions, and lambdas with closures.

```lisp
; Quicksort
(load lib)

(defun qsort (()) ())
(defun qsort ((:: pivot xs))
  (let (greater (comp x (<- x xs) (> x pivot)))
  (let (lesser  (comp x (<- x xs) (<= x pivot)))
    (append (qsort lesser) (:: pivot (qsort greater))))))

(qsort (list 3 1 4 1 5 9 2 6))
; => (1 1 2 3 4 5 6 9)
```

## Build & Run

```sh
cc -std=c11 -O2 -o hazel hazel.c
./hazel examples/test.lsp
```

## How hazel differs from Lisp

### 1. Lazy `::` — infinite lists for free

The tail of `(:: h t)` is not evaluated until someone looks at it.
Any recursive function built with `::` is automatically a stream:

```lisp
(load lib)

(defun double (x) (* x 2))
(take 10 (range 1 1000000000))      ; => (1 2 3 4 5 6 7 8 9 10)
(take 10 (map double (range 1 1000000000)))
                                    ; => (2 4 6 8 10 12 14 16 18 20)

(letrec (ones (:: 1 ones))          ; a list that contains itself
  (take 5 ones))                    ; => (1 1 1 1 1)

(defun add (a b) (+ a b))
(define fib (:: 0 (:: 1 (map add fib (cdr fib)))))
(take 10 fib)                       ; => (0 1 1 2 3 5 8 13 21 34)
```

`range` is a lazy builtin; `map`, `filter`, and `zip` are defined in hazel
itself (`examples/lib.lsp`) and are just as lazy. See `examples/sieve.lsp` and
`examples/hamming.lsp` for classic lazy programs.

### 2. Pattern-matching function clauses (no `cond` ladders)

`defun` may be repeated; clauses are tried in definition order, matching on
literals, `()`/`nil`, `_`, `(:: h t)`, `(cons a b)`, and `(list ...)` patterns:

```lisp
(defun fib (0) 0)
(defun fib (1) 1)
(defun fib (n) (+ (fib (- n 1)) (fib (- n 2))))
(fib 10)                ; => 55

(defun sum (()) 0)
(defun sum ((:: x xs)) (+ x (sum xs)))
(sum (list 1 2 3 4 5))  ; => 15

(defun gcd (a 0) a)
(defun gcd (a b) (gcd b (mod a b)))
(gcd 16 28)             ; => 4
```

`let` destructures too:

```lisp
(let ((cons a b) (cons 3 7)) (+ a b))   ; => 10
```

This is also where `define` and `defun` part ways: `(defun fib (n) (+ (fib
(- n 1)) (fib (- n 2))))` defines a *function* — each call re-runs the body
from scratch. `(define fib (:: 0 (:: 1 (map add fib (cdr fib)))))`
instead defines a *value*: `fib` is bound once to an actual (lazy) pair
chain, and since `define` puts the name in scope before evaluating its
right-hand side, that chain can refer to itself by name while being built.
Later reads of `fib` reuse the same structure instead of recomputing it.

### 3. List comprehensions

`(comp expr generators/guards...)` with Haskell-style `<-` generators.
`comp` is sugar: it desugars to `lambda` + `concatmap`, so it needs
`(load lib)`:

```lisp
(load lib)

(comp (* x x) (<- x (range 1 10)) (even x))   ; => (4 16 36 64 100)

; Pythagorean triples
(comp (list a b c)
  (<- a (range 1 15))
  (<- b (range a 15))
  (<- c (range b 15))
  (= (+ (* a a) (* b b)) (* c c)))
; => ((3 4 5) (5 12 13) (6 8 10) (9 12 15))
```

### 4. `lambda` with closures; operators are first-class

`(lambda (params) body)` captures its environment (parameters may be
patterns), and operators are values, so both of these work:

```lisp
(load lib)

(map (lambda (x) (* x x)) (range 1 5))   ; => (1 4 9 16 25)
(foldr + 0 (range 1 10))                 ; => 55

(define add (lambda (n) (lambda (x) (+ x n))))
((add 3) 4)                              ; => 7 (curried closure)
```

### 5. Purely functional

No `set!` / `setq` — values are immutable. Named functions live in a global
clause table; `define` binds global constants; `let` / `letrec` bind locally.
`letrec` allows a binding to refer to itself, which combined with laziness
gives cyclic streams (see the Fibonacci example above).

### 6. S-expression printing

Lists print as `(1 2 3)`, and `nil`/`()` prints as `nil`. A pair prints
dotted, `(a . b)`, only when its cdr is not itself a pair — dotted notation
reflects whatever the cdr chain actually ends in, not how the pair was
built, so `(cons (list 1 2) (list 3 4))` prints as `((1 2) 3 4)`, not
dotted, because its cdr happens to be a proper list. Top-level expressions
are evaluated and printed REPL-style, one per line.

### 7. Proper tail calls

The evaluator loops instead of recursing on tail calls, so tail-recursive
functions run in constant stack space (`examples/tco.lsp`).

### 8. `cond`, alongside `if`

The original `(cond (test1 e1) (test2 e2) ...)` multi-clause form works
side by side with `(if c then else)` — the two aren't mutually exclusive.

## Language reference

### Special forms

| Form | Meaning |
|---|---|
| `(defun name (pats...) body)` | add a function clause |
| `(define name expr)` | global constant (may self-reference) |
| `(let (pat expr) body)` | local binding with destructuring |
| `(letrec (name expr) body)` | self-referential (lazy) binding |
| `(if c then else)` | conditional (0 is false) |
| `(cond (t1 e1) (t2 e2) ...)` | multi-clause conditional |
| `(and a b)` `(or a b)` | booleans (`true` = 1, `false` = 0) — short-circuiting, so these must be special forms |
| `(:: h t)` | lazy list cell; tail is deferred |
| `(lambda (pats...) body)` | anonymous function, captures environment |
| `(comp expr gens...)` | list comprehension (sugar; needs `(load lib)`) |
| `(quote x)` / `'x` | literal |
| `(begin e1 e2 ...)` | sequence |
| `(load name)` | load `name.lsp` from the script's directory |

### Builtins

`+ - * / mod` `< > = <= >= /=` (numbers are C doubles) ·
`list cons` · `car cdr` · `eq` · `atom` · `not` ·
`range` (lazy) ·
`printf` (C-style: `%v` value, `%s` char-code list as text, `%d` number, `%%` literal `%`)

`not`, unlike `and`/`or`, doesn't need to short-circuit, so it's an
ordinary builtin rather than a special form.

Operators are first-class values: `(foldr + 0 xs)` works directly.
`null`, `append` (lazy), `length`, `map`, `filter`, `zip`, and `concatmap` live in `examples/lib.lsp`,
written in hazel itself; user `defun`s take precedence over builtins, so any
builtin can be redefined.

String literals like `"hi"` are sugar for quoted lists of character codes;
`printf`'s `%s` prints them back as text. No newline is added automatically:

```lisp
(load lib)

(printf "%s has %d items\n" "abc" (length "abc"))   ; abc has 3 items
```

## Examples

The `examples/` directory doubles as the test suite — start with
`examples/test.lsp`. Highlights:

- `sieve.lsp` — lazy Sieve of Eratosthenes
- `hamming.lsp` — Hamming numbers via three merged infinite streams
- `queens.lsp` — N-queens via pattern-matching recursion
- `prolog.lsp`, `miniKanren.lsp` — logic programming in hazel
- `lib.lsp` — the standard library (`take`, `foldr`, `any`, `all`, ...), pulled in with `(load lib)`

## Limitations

- **Garbage collector** — a single-generation mark-sweep collector, rooted
  only at the global environment, that runs once per top-level form and
  never mid-evaluation. That timing makes it safe for laziness: nothing is
  ever swept while a form is still being evaluated, and any not-yet-forced
  lazy tail — together with the whole local environment it closed over —
  survives across collections for as long as it stays reachable from a
  global binding (`define`/`defun`), since marking walks into a deferred
  tail's expression and environment the same way it walks pairs and
  closures. The real risk is structural rather than laziness-specific:
  marking a list recurses one C stack frame per pair with no explicit
  trampoline, so a very long list that has been fully realized (forced
  end-to-end) and is still reachable from the global environment — e.g.
  `(define biglist (range 1 1000000))` after something has walked all of
  it — can overflow the stack during collection. Lazy streams make this
  easier to hit in practice than in a typical hand-written Lisp program,
  since `range`/`::` invite building lists far longer than one would
  normally type out by hand.
- Single numeric type (double); no string or char types beyond char-code lists.
- No macros.
