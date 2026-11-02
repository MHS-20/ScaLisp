package scalisp

import scala.annotation.tailrec

object Eval:
  private val specialForms =
    Set("quote", "quasiquote", "if", "define", "defmacro", "lambda", "set!", "begin")

  def run(source: String, env: Env): Value =
    Reader.readAll(source).map(eval(_, env)).lastOption.getOrElse(Nil_)

  // Sub-expressions go through `sub`, so only tail positions are self-calls and
  // @tailrec turns them into a loop: tail-recursive Lisp code runs in constant stack.
  private def sub(x: Value, env: Env): Value = eval(x, env)

  @tailrec def eval(x: Value, env: Env): Value = x match
    case Sym(name) => env(name)

    case Lst(Sym("quote") :: v :: Nil)      => v
    case Lst(Sym("quasiquote") :: v :: Nil) => quasi(v, env)

    case Lst(Sym("if") :: c :: t :: e :: Nil) => if sub(c, env).truthy then eval(t, env) else eval(e, env)
    case Lst(Sym("if") :: c :: t :: Nil)      => if sub(c, env).truthy then eval(t, env) else Nil_

    case Lst(Sym("define") :: Sym(name) :: v :: Nil) =>
      env.define(name, sub(v, env))
      Sym(name)
    case Lst(Sym("define") :: Lst(Sym(name) :: params) :: body) =>
      env.define(name, closure(params, body, env))
      Sym(name)
    case Lst(Sym("defmacro") :: Sym(name) :: Lst(params) :: body) =>
      env.define(name, Macro(closure(params, body, env)))
      Sym(name)
    case Lst(Sym("lambda") :: Lst(params) :: body) => closure(params, body, env)

    case Lst(Sym("set!") :: Sym(name) :: v :: Nil) =>
      val value = sub(v, env)
      env.set(name, value)
      value

    case Lst(Sym("begin") :: Nil) => Nil_
    case Lst(Sym("begin") :: body) =>
      body.init.foreach(sub(_, env))
      eval(body.last, env)

    case Lst(Sym(s) :: _) if specialForms(s) => throw LispError(s"malformed special form: ${x.show}")

    case Lst(Nil) => x

    case Lst(head :: args) =>
      sub(head, env) match
        case Macro(c)      => eval(applyTo(c, args), env)
        case Prim(_, fn)   => fn(args.map(sub(_, env)))
        case c: Closure    =>
          val frame = bind(c, args.map(sub(_, env)))
          c.body.init.foreach(sub(_, frame))
          eval(c.body.last, frame)
        case other         => throw LispError(s"not a function: ${other.show}")

    case _ => x

  def apply(f: Value, args: List[Value]): Value = f match
    case Prim(_, fn) => fn(args)
    case c: Closure  => applyTo(c, args)
    case other       => throw LispError(s"not a function: ${other.show}")

  private def applyTo(c: Closure, args: List[Value]): Value =
    val frame = bind(c, args)
    c.body.map(sub(_, frame)).last

  private def bind(c: Closure, args: List[Value]): Env =
    val arityOk = if c.rest.isDefined then args.length >= c.params.length else args.length == c.params.length
    if !arityOk then throw LispError(s"expected ${c.params.length} arguments, got ${args.length}")
    val frame = Env(Some(c.env))
    c.params.zip(args).foreach(frame.define)
    c.rest.foreach(frame.define(_, Lst(args.drop(c.params.length))))
    frame

  private def closure(params: List[Value], body: List[Value], env: Env): Closure =
    if body.isEmpty then throw LispError("lambda needs a body")
    val names = params.map:
      case Sym(n) => n
      case other  => throw LispError(s"parameter is not a symbol: ${other.show}")
    names.indexOf("&rest") match
      case -1 => Closure(names, None, body, env)
      case i if i == names.length - 2 => Closure(names.take(i), Some(names.last), body, env)
      case _ => throw LispError("&rest must precede exactly one final parameter")

  // ponytail: nested quasiquotes are not depth-tracked; an inner unquote evaluates immediately.
  private def quasi(x: Value, env: Env): Value = x match
    case Lst(Sym("unquote") :: v :: Nil) => sub(v, env)
    case Lst(items) =>
      Lst(items.flatMap:
        case Lst(Sym("unquote-splicing") :: v :: Nil) =>
          sub(v, env) match
            case Lst(spliced) => spliced
            case other        => throw LispError(s"cannot splice non-list: ${other.show}")
        case item => List(quasi(item, env))
      )
    case _ => x
