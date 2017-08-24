import atto._, Atto._

sealed trait Expr
case class ELit(value: Int) extends Expr
case class EVar(name: String) extends Expr
case class EPlus(x: Expr, y: Expr) extends Expr

sealed trait Stmt
case class SAssign(varname: String, e: Expr) extends Stmt
case class SPrint(e: Expr) extends Stmt

case class Context(values: Map[String, Int])

object Expr0 {
  val initCtx: Context = Context(Map.empty)

  def evalute(e: Expr, context: Context): Option[Int] = e match {
    case ELit(v) => Option(v)
    case EVar(n) => context.values.get(n)
    case EPlus(x, y) => for {
      xv <- Expr0.evalute(x, context)
      yv <- Expr0.evalute(y, context)
    } yield xv + yv
  }

  def interpret(s: Stmt, c: Context): Option[Context] = s match {
    case SAssign(name, expr) => for {
      value <- evalute(expr, c)
    } yield Context(c.values + ((name, value)))
    case SPrint(expr) => for {
      value <- evalute(expr, c)
      _ = println(value)
    } yield c
  }

  def runProgram(prog: List[Stmt]): Unit =
    prog.foldLeft(Option(initCtx)) {
      case (Some(ctx), stmt) => interpret(stmt, ctx)
      case (None, _) => {
        println("error")
        None
      }
    }

  def test1: Option[Int] = {
    val ctx = Context(Map(("a", 2), ("b", 3)))
    val e = EPlus(EVar("b"), EVar("a"))
    Expr0.evalute(e, ctx)
  }

  def test2: Option[Int] = {
    val str = "1+1+(2+x)"
    val ctx = Context(Map(("x", 4)))
    val eo = (ExprParser.expr parseOnly str).option
    for {
      e <- eo
      result <- evalute(e, ctx)
    } yield result
  }

  def test3: Unit = {
    val str = "x=1;print x;x=x+1;print x"
    val r = ExprParser.program parseOnly str
    val p = r.option.getOrElse(List())
    runProgram(p)
  }
}
