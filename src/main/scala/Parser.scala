import atto._, Atto._
import scala.io.Source
import cats.implicits._

object ExprParser {
  def variable: Parser[String] = for {
    c <- letter
    cs <- stringOf(letterOrDigit)
  } yield c.toString ++ cs

  def expr: Parser[Expr] = for {
    terms <- myterm sepBy string("+")
    n: Expr = ELit(0)
  } yield terms.foldLeft(n)({ EPlus(_,_) })

  def myterm: Parser[Expr] = {
    val n:Parser[Expr] = int.map({ ELit(_) })
    val v:Parser[Expr] = variable.map({ EVar(_) })
    val p:Parser[Expr] = for {
      _ <- string("(")
      e <- expr
      _ <- string(")")
    } yield e
    orElse(n, orElse(v, p))
  }

  def stmt: Parser[Stmt] = {
    val assign:Parser[Stmt] = for {
      v <- variable
      _ <- string("=")
      e <- expr
    } yield SAssign(v, e)
    val pr:Parser[Stmt] = for {
      _ <- string("print ")
      e <- expr
    } yield SPrint(e)
    orElse(assign, pr)
  }

  def program: Parser[List[Stmt]] = stmt sepBy (string(";"))

  def programFromFile(path: String): Option[List[Stmt]] = {
    val f = Source.fromFile("sample.txt")
    val ls = f.getLines.toList
    ls.map( line => (stmt parseOnly line).option).sequence
  }
}
