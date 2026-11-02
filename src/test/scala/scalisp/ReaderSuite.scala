package scalisp

import org.scalatest.funsuite.AnyFunSuite

class ReaderSuite extends AnyFunSuite:
  test("atoms and nested lists") {
    assert(Reader.readAll("(a -1 (\"s\")) ; note") ===
      List(Lst(List(Sym("a"), Num(-1), Lst(List(Str("s")))))))
  }

  test("quote shorthands expand") {
    assert(Reader.readAll("`(,a ,@b 'c)").head.show ===
      "(quasiquote ((unquote a) (unquote-splicing b) (quote c)))")
  }

  test("string escapes round-trip") {
    val form = Reader.readAll(""""a\"b\n\\"""").head
    assert(form === Str("a\"b\n\\"))
    assert(Reader.readAll(form.show).head === form)
  }

  test("open forms are incomplete") {
    Seq("(a", "'", "\"abc", "\"abc\\\"").foreach(s => assertThrows[Incomplete](Reader.readAll(s)))
  }

  test("stray close paren is an error") {
    val e = intercept[LispError](Reader.readAll(")"))
    assert(!e.isInstanceOf[Incomplete])
  }
