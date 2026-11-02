package scalisp

import scala.io.Source

object Builtins:
  private val T = Sym("t")
  private def bool(b: Boolean): Value = if b then T else Nil_

  private def nums(args: List[Value]): List[BigInt] = args.map:
    case Num(n) => n
    case other  => throw LispError(s"not a number: ${other.show}")

  private def nonZero(n: BigInt): BigInt =
    if n == 0 then throw LispError("division by zero") else n

  private def compare(op: (BigInt, BigInt) => Boolean)(args: List[Value]): Value =
    bool(nums(args).sliding(2).forall { case List(a, b) => op(a, b); case _ => true })

  private val primitives: Map[String, PartialFunction[List[Value], Value]] = Map(
    "+" -> { case a => Num(nums(a).sum) },
    "*" -> { case a => Num(nums(a).product) },
    "-" -> {
      case List(a)     => Num(-nums(List(a)).head)
      case a @ _ :: _  => Num(nums(a).reduceLeft(_ - _))
    },
    "/" -> { case a @ _ :: _ :: _ => Num(nums(a).reduceLeft((x, y) => x / nonZero(y))) },
    "mod" -> { case List(Num(a), Num(b)) => Num(a.mod(nonZero(b).abs)) },
    "=" -> compare(_ == _),
    "<" -> compare(_ < _),
    ">" -> compare(_ > _),
    "<=" -> compare(_ <= _),
    ">=" -> compare(_ >= _),
    "cons" -> { case List(h, Lst(t)) => Lst(h :: t) },
    "car" -> { case List(Lst(l)) => l.headOption.getOrElse(Nil_) },
    "cdr" -> { case List(Lst(l)) => Lst(l.drop(1)) },
    "list" -> { case a => Lst(a) },
    "null?" -> { case List(v) => bool(v == Nil_) },
    "atom?" -> { case List(v) => bool(!v.isInstanceOf[Lst] || v == Nil_) },
    "eq?" -> { case List(a, b) => bool(a == b) },
    "apply" -> { case List(f, Lst(a)) => Eval(f, a) },
    "print" -> {
      case a =>
        println(a.map { case Str(s) => s; case v => v.show }.mkString(" "))
        Nil_
    },
    "gensym" -> { case Nil => gensym() },
    "error" -> { case List(Str(m)) => throw LispError(m) }
  )

  private var gensymCount = 0
  private def gensym(): Value =
    gensymCount += 1
    Sym(s"#g$gensymCount")

  /** A fresh global environment holding the primitives and the Lisp-defined prelude. */
  def globalEnv(): Env =
    val env = Env()
    env.define("t", T)
    env.define("nil", Nil_)
    primitives.foreach: (name, fn) =>
      env.define(name, Prim(name, args =>
        fn.applyOrElse(args, _ => throw LispError(s"bad arguments to $name: ${Lst(args).show}"))
      ))
    val prelude = Source.fromResource("prelude.lisp")
    try Eval.run(prelude.mkString, env)
    finally prelude.close()
    env
