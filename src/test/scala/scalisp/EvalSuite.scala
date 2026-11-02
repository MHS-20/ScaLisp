package scalisp

import org.scalatest.funsuite.AnyFunSuite

class EvalSuite extends AnyFunSuite:
  private def run(src: String): String = Eval.run(src, Builtins.globalEnv()).show

  private def fails(src: String): Unit = assertThrows[LispError](run(src))

  test("arithmetic is arbitrary precision") {
    assert(run("(define (f n) (if (= n 0) 1 (* n (f (- n 1))))) (f 25)") === "15511210043330985984000000")
    assert(run("(- 5)") === "-5")
    assert(run("(mod -7 3)") === "2")
  }

  test("closures capture their environment") {
    assert(run("""
      (define (counter)
        (let ((n 0)) (lambda () (set! n (+ n 1)) n)))
      (define c (counter))
      (c) (c)""") === "2")
  }

  test("rest parameters") {
    assert(run("((lambda (a &rest r) (list a r)) 1 2 3)") === "(1 (2 3))")
    assert(run("((lambda (a &rest r) r) 1)") === "()")
  }

  test("tail calls run in constant stack") {
    assert(run("(define (loop n) (if (= n 0) 'done (loop (- n 1)))) (loop 1000000)") === "done")
  }

  test("prelude list functions") {
    assert(run("(map (lambda (x) (* x x)) '(1 2 3))") === "(1 4 9)")
    assert(run("(filter (lambda (x) (> x 1)) '(1 2 3))") === "(2 3)")
    assert(run("(append '(1 2) '(3))") === "(1 2 3)")
    assert(run("(length '(a b))") === "2")
  }

  test("quasiquote splices and macros expand before evaluation") {
    assert(run("(define x 2) `(1 ,x ,@(list 3 4))") === "(1 2 3 4)")
    assert(run("(defmacro unless (c &rest b) `(if ,c nil (begin ,@b))) (unless nil 1 2)") === "2")
  }

  test("macros in the prelude") {
    assert(run("(cond (nil 1) ((= 1 1) 2) (t 3))") === "2")
    assert(run("(and 1 2)") === "2")
    assert(run("(and 1 nil (error \"not reached\"))") === "()")
    assert(run("(or nil 2 (error \"not reached\"))") === "2")
    assert(run("(define n 0) (or (begin (set! n (+ n 1)) n) 9) n") === "1")
    assert(run("(let ((a 1) (b 2)) (+ a b))") === "3")
  }

  test("errors") {
    fails("missing")
    fails("(1 2)")
    fails("(car 1)")
    fails("(/ 1 0)")
    fails("(mod 1 0)")
    fails("(+ 1 'a)")
    fails("((lambda (a) a))")
    fails("(if)")
    fails("(lambda (a))")
    fails("(lambda (&rest) 1)")
    fails("(set! missing 1)")
  }
