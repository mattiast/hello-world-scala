import scala.util.parsing.combinator.Parsers

sealed trait Expr
case class ELit(value: Int) extends Expr
case class EVar(name: String) extends Expr
case class EPlus(x: Expr, y: Expr) extends Expr

object Expr0 {
  def evalute(e: Expr, context: Map[String, Int]): Option[Int] = e match {
    case ELit(v) => Option(v)
    case EVar(n) => context.get(n)
    case EPlus(x, y) => for {
      xv <- Expr0.evalute(x, context)
      yv <- Expr0.evalute(y, context)
    } yield xv + yv
  }

  def test1: Option[Int] = {
    val ctx = Map(("a", 2), ("b", 3))
    val e = EPlus(EVar("b"), EVar("a"))
    Expr0.evalute(e, ctx)
  }

  def test2: Option[Int] = {
    val str = "1+1+(2+x)"
    val ctx = Map(("x", 4))
    val eo = ExprParser.parseAll(ExprParser.expr, str).map({Option(_)}).getOrElse(None)
    for {
      e <- eo
      result <- evalute(e, ctx)
    } yield result
  }
}


