package scalisp

object Main:
  def main(args: Array[String]): Unit =
    val env = Builtins.globalEnv()
    args.foreach(Repl.loadFile(_, env))
    Repl.launch(env)
