package scalisp

import scala.io.StdIn
import scala.util.{Failure, Success, Using}

object Repl:
  /** Runs one piece of source, reporting failures instead of ending the session. */
  private def safely(action: => Unit): Unit =
    try action
    catch
      case e: LispError          => println(s"error: ${e.getMessage}")
      case _: StackOverflowError => println("error: stack overflow")

  def loadFile(path: String, env: Env): Unit =
    Using(scala.io.Source.fromFile(path))(_.mkString) match
      case Success(source) => safely(Eval.run(source, env))
      case Failure(e)      => println(s"error: cannot load $path: ${e.getMessage}")

  def launch(env: Env): Unit =
    var buffer = ""
    var line = StdIn.readLine("> ")
    while line != null do
      buffer += line + "\n"
      try
        val forms = Reader.readAll(buffer)
        buffer = ""
        forms.foreach(form => safely(println(Eval.eval(form, env).show)))
      catch
        case _: Incomplete => ()
        case e: LispError =>
          buffer = ""
          println(s"error: ${e.getMessage}")
      line = StdIn.readLine(if buffer.isEmpty then "> " else "|  ")
    println()
