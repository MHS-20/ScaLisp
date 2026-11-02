package scalisp

import scala.collection.mutable

class LispError(message: String) extends Exception(message)

sealed trait Value:
  def truthy: Boolean = this != Nil_

  /** Readable form: strings are quoted, so `show` output reads back as data. */
  def show: String = this match
    case Num(n)        => n.toString
    case Sym(name)     => name
    case Str(s)        => "\"" + s.flatMap(Str.escape) + "\""
    case Lst(items)    => items.map(_.show).mkString("(", " ", ")")
    case Prim(name, _) => s"#<primitive $name>"
    case _: Closure    => "#<lambda>"
    case _: Macro      => "#<macro>"

case class Num(value: BigInt) extends Value
case class Sym(name: String) extends Value
case class Str(value: String) extends Value
// ponytail: proper lists only, no dotted pairs; cons needs a list as its tail.
case class Lst(items: List[Value]) extends Value
case class Prim(name: String, fn: List[Value] => Value) extends Value
case class Closure(params: List[String], rest: Option[String], body: List[Value], env: Env) extends Value
case class Macro(closure: Closure) extends Value

object Str:
  def escape(c: Char): String = c match
    case '"'  => "\\\""
    case '\\' => "\\\\"
    case '\n' => "\\n"
    case '\t' => "\\t"
    case c    => c.toString

val Nil_ : Value = Lst(Nil)

class Env(parent: Option[Env] = None):
  private val vars = mutable.Map.empty[String, Value]

  def define(name: String, value: Value): Unit = vars(name) = value

  def apply(name: String): Value =
    vars.get(name).orElse(parent.map(_(name))).getOrElse:
      throw LispError(s"unbound symbol: $name")

  def set(name: String, value: Value): Unit =
    if vars.contains(name) then vars(name) = value
    else parent match
      case Some(p) => p.set(name, value)
      case None    => throw LispError(s"unbound symbol: $name")
