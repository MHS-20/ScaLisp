package scalisp

import scala.annotation.tailrec

/** Input ended inside an open form; the REPL reads another line when it sees this. */
class Incomplete extends LispError("unexpected end of input")

object Reader:
  private val tokenRe = """,@|[()'`,]|"(?:\\.|[^"\\])*"?|;[^\n]*|[^\s()'`,";]+""".r
  private val stringRe = """(?s)"(?:\\.|[^"\\])*"""".r
  private val numberRe = """-?\d+""".r
  private val escapeRe = """(?s)\\(.)""".r

  private val quotes = Map(
    "'" -> "quote",
    "`" -> "quasiquote",
    "," -> "unquote",
    ",@" -> "unquote-splicing"
  )

  def readAll(source: String): List[Value] =
    val tokens = tokenRe.findAllIn(source).filterNot(_.startsWith(";")).toList

    @tailrec def loop(tokens: List[String], acc: List[Value]): List[Value] =
      if tokens.isEmpty then acc.reverse
      else
        val (form, rest) = readForm(tokens)
        loop(rest, form :: acc)

    loop(tokens, Nil)

  private def readForm(tokens: List[String]): (Value, List[String]) = tokens match
    case Nil      => throw Incomplete()
    case "(" :: t => readList(t, Nil)
    case ")" :: _ => throw LispError("unexpected )")
    case q :: t if quotes.contains(q) =>
      val (form, rest) = readForm(t)
      (Lst(List(Sym(quotes(q)), form)), rest)
    case t :: rest => (atom(t), rest)

  @tailrec private def readList(tokens: List[String], acc: List[Value]): (Value, List[String]) =
    tokens match
      case Nil        => throw Incomplete()
      case ")" :: t   => (Lst(acc.reverse), t)
      case _          =>
        val (form, rest) = readForm(tokens)
        readList(rest, form :: acc)

  private def atom(token: String): Value =
    if token.startsWith("\"") then
      if !stringRe.matches(token) then throw Incomplete()
      Str(escapeRe.replaceAllIn(token.drop(1).dropRight(1), m =>
        scala.util.matching.Regex.quoteReplacement(m.group(1) match
          case "n" => "\n"
          case "t" => "\t"
          case c   => c
        )))
    else if numberRe.matches(token) then Num(BigInt(token))
    else Sym(token)
