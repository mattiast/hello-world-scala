import scala.util.parsing.combinator.RegexParsers
import scala.io.Source
import cats.implicits._

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

  def stmt: Parser[Stmt] = variable ~ "=" ~ expr ^^ {
    case v ~ _ ~ e => SAssign(v, e)
  } | "print " ~ expr ^^ { case _ ~ e => SPrint(e) }

  def program: Parser[List[Stmt]] = repsep(stmt, ";")

  def programFromFile(path: String): Option[List[Stmt]] = {
    val f = Source.fromFile("sample.txt")
    val ls = f.getLines.toList
    ls.map( line => parseAll(stmt, line).map({ Option(_) }).getOrElse(None) ).sequence
  }
}
