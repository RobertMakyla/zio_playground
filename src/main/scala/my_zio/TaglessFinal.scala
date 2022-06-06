package my_zio
import scala.io.StdIn
object TaglessFinalTest extends App {

  /** Algebra represents the DSL
   *
   * In the examples we often see "F[_] : Monad"
   * - we use F[_] (or any higher kinded type - wrapper Expr[_]) for which there is a Monad[F]
   * - monad so that we can use it in for-comprehension
   */
  trait MyAlgebra[F[_]] {
    def b(v: Boolean): F[Boolean] // create (ScalaType => F[StalaType])

    def i(v: Int): F[Int]

    def or(left: F[Boolean], right: F[Boolean]): F[Boolean] // Ops (F[StalaType] => F[StalaType])

    def and(left: F[Boolean], right: F[Boolean]): F[Boolean]

    def sum(left: F[Int], right: F[Int]): F[Int]
  }

  /**
   * Algebra - represents the abstract DSL with higher kinded generic type F[_], might be implemented with several types with different types F[_]
   * Interpreter - is an implementation of an Algebra (F[T] becomes Either[Throwable, T] ). Having F[T], Interpreted makes F concrete
   * Program - client of Albegra, implements the business logic (sequence of DSL operations).
   */

  case class SimpleExpr[E](value: E)

  // Interpreter no. 1
  implicit val simpleInterpreter: MyAlgebra[SimpleExpr] = new MyAlgebra[SimpleExpr] {
    override def b(v: Boolean): SimpleExpr[Boolean] = SimpleExpr(v)
    override def i(v: Int): SimpleExpr[Int] = SimpleExpr(v)
    override def or(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] = SimpleExpr(left.value || right.value)
    override def and(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] = SimpleExpr(left.value && right.value)
    override def sum(left: SimpleExpr[Int], right: SimpleExpr[Int]): SimpleExpr[Int] = SimpleExpr(left.value + right.value)
  }

  type MyEither[E] = Either[String, E]

  // Interpreter no. 2
  implicit val eitherInterpreter: MyAlgebra[MyEither] = new MyAlgebra[MyEither] {
    override def b(v: Boolean): MyEither[Boolean] = Right(v)
    override def i(v: Int): MyEither[Int] = Right(v)
    override def or(left: MyEither[Boolean], right: MyEither[Boolean]): MyEither[Boolean] = (left, right) match {
      case (Right(l), Right(r)) => Right(l || r)
      case (Left(_), Right(r)) => Right(r)
      case (Right(l), Left(_)) => Right(l)
      case (Left(e1), Left(e2)) => Left(e1 + " " + e2)
    }
    override def and(left: MyEither[Boolean], right: MyEither[Boolean]): MyEither[Boolean] = (left, right) match {
      case (Right(l), Right(r)) => Right(l && r)
      case (Left(_), Right(r)) => Right(r)
      case (Right(l), Left(_)) => Right(l)
      case (Left(e1), Left(e2)) => Left(e1 + " " + e2)
    }
    override def sum(left: MyEither[Int], right: MyEither[Int]): MyEither[Int] = (left, right) match {
      case (Right(l), Right(r)) => Right(l + r)
      case (Left(_), Right(r)) => Right(r)
      case (Right(l), Left(_)) => Right(l)
      case (Left(e1), Left(e2)) => Left(e1 + " " + e2)
    }
  }


  /**
   * PROGRAM is just a sequence of operations on the DSL
   * Until we provide an INTERPRETER, it doesn't do anything
   */
  def program1[E[Boolean]](implicit alg: MyAlgebra[E]) = {
    import alg._
    or(b(false), or(b(true), and(b(false), b(true))))
  }

  println(program1(simpleInterpreter))
  println(program1(eitherInterpreter))
}
