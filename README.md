# ScaLisp

A Lisp interpreter written in Scala 3, with lexical closures, `defmacro`, quasiquote, tail-call elimination and arbitrary-precision integers. The reader and evaluator use only the Scala standard library.

## Setup

[mise](https://mise.jdx.dev) provides the JDK and sbt, pinned per project in `mise.toml`. Nothing needs to be installed globally.

```shell
mise trust && mise install
mise exec -- sbt test
mise exec -- sbt run                          # start the REPL
mise exec -- sbt "run examples/fizzbuzz.lisp" # load files, then start the REPL
```

Press Ctrl-D to leave the REPL. A form can span several lines; the prompt changes to `|` until its parentheses balance.

## Language

Only `nil`, the empty list, is false. `t` is the conventional true value.

| Form | Meaning |
|---|---|
| `(quote x)`, `'x` | `x`, unevaluated |
| `` `(a ,b ,@c) `` | list template with unquote and splicing |
| `(if c a [b])` | conditional |
| `(define name v)`, `(define (f a) ...)` | bind in the current scope |
| `(lambda (a &rest r) ...)` | function; `&rest` collects extra arguments |
| `(defmacro m (a) ...)` | function from unevaluated forms to a form |
| `(set! name v)` | assign to an existing binding |
| `(begin ...)` | evaluate in order, return the last value |

Primitives: `+ - * / mod = < > <= >= cons car cdr list null? atom? eq? apply print gensym error`.

The prelude adds `not cadr foldl reverse length map filter append` and the macros `let cond and or`.

Example:

```lisp
> (defmacro unless (c &rest body) `(if ,c nil (begin ,@body)))
unless
> (unless nil 'ran)
ran
```

Limits: lists are always proper (no dotted pairs), integers are the only numbers, and nested quasiquotes are not depth-tracked.

## How it works

- `Reader.scala` tokenizes with one regex and builds values by recursive descent. It reports unfinished input separately from errors, which is how the REPL knows to keep reading.
- `Eval.scala` evaluates forms. Tail positions are self-calls that `@tailrec` compiles to a loop, so tail-recursive Lisp code runs in constant stack. Non-tail recursion uses the JVM stack, and the REPL reports overflow as an error.
- Macros receive their arguments unevaluated, and their result is then evaluated in the caller's environment.
- `Builtins.scala` holds the primitives and loads `src/main/resources/prelude.lisp`, which defines everything that needs no interpreter support.
