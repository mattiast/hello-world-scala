import scala.util.parsing.combinator.RegexParsers

object ExprParser extends RegexParsers {
  def number: Parser[Int] = """-?\d+""".r ^^ { _.toInt }
  def variable: Parser[String] = """[a-zA-Z][a-zA-Z0-9]*""".r

  def expr: Parser[Expr] = myterm ~ rep( "+" ~ myterm ) ^^ {
    case t0 ~ rest => rest.foldLeft(t0) {
      case (t1, "+" ~ t2) => EPlus(t1, t2)
    }
  }

  def myterm: Parser[Expr] = number ^^ { ELit(_) } | variable ^^ { EVar(_) } | "(" ~ expr ~ ")" ^^ {
    case _ ~ e ~ _ => e
  }
}
