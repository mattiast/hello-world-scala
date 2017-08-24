sealed trait Expr
case class ELit(value: Int) extends Expr
case class EPlus(x: Expr, y: Expr) extends Expr

object Expr0 {
  def jou: String = "jou"
  def evalute(e: Expr): Int = e match {
    case ELit(v) => v
    case EPlus(x, y) => Expr0.evalute(x) + Expr0.evalute(y)
  }
}


