import scala.collection.immutable.HashSet

object Main extends App {
  val p = ExprParser.programFromFile("sample.txt")
  p.fold(())(Expr0.runProgram)
}

object Foo {
  def f(x: Int): Int = x + 1
  def last(list: List[Int]): Option[Int] = list match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: y => Foo.last(y)
  }

  def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case _ :: y => Foo.length(y) + 1
  }

  def reverse[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case x :: y => Foo.reverse(y) ++ List(x)
  }

  def inl[A, B](x: A): Either[A,B] =
    Left(x)

  def extr[A](x: Either[A, Nothing]): A = x match {
    case Left(a) => a
    case Right(n) => n
  }

  def sum(x: List[Int]): Int = x.fold(0){ (x,y) => x+y }


  def xx = {
    val s = HashSet(1,2,3)
    s.fold(0)( (x,y) => x + y )
  }
}
