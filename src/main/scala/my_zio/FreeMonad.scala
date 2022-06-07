package my_zio

import cats.free.Free
import cats.free.Free.liftF

object FreeMonad extends App {

  /** Algebra represents the DSL
   *
   * The following types are ADT (Algebric Data Types)
   * MyAlgebra[A]   - where 'A' represents the result type
   *
   * source: https://typelevel.org/cats/datatypes/freemonad.html
   */
  sealed trait MyAlgebra[A]

  case class B(a: Boolean) extends MyAlgebra[Boolean]
  case class I(a: Int) extends MyAlgebra[Int]
  case class Or(a: Boolean, b: Boolean) extends MyAlgebra[Boolean]
  case class And(a: Boolean, b: Boolean) extends MyAlgebra[Boolean]
  case class Sum(a: Int, b: Int) extends MyAlgebra[Int]

  /**
   * 1. Create a type based on Free and your Algebra
   */
  type MyAlgebraFree[A] = Free[MyAlgebra, A]

  /**
   * 2. Create CONSTRUCTORS with liftF() function
   */
  def b(a: Boolean): MyAlgebraFree[Boolean] = liftF[MyAlgebra, Boolean](B(a))
  def i(a: Int): MyAlgebraFree[Int] = liftF[MyAlgebra, Int](I(a))
  def or(a: Boolean, b: Boolean): MyAlgebraFree[Boolean] = liftF[MyAlgebra, Boolean](Or(a, b))
  def sum(a: Boolean, b: Boolean): MyAlgebraFree[Boolean] = liftF[MyAlgebra, Boolean](And(a, b))
  def and(a: Int, b: Int): MyAlgebraFree[Int] = liftF[MyAlgebra, Int](Sum(a, b))

  /**
   * 3. PROGRAM (sequence of DSL operations - doesn't do anything)
   */
  def program: MyAlgebraFree[Boolean] = {
    for {
      b1 <- b(false)
      b2 <- b(true)
      res <- or(b1, b2)
    } yield res
  }

  /**
   * 4. COMPILER (interpreter)
   */

  import cats.{Id, ~>}

  def compiler: MyAlgebra ~> Id = new (MyAlgebra ~> Id) {

    def apply[A](fa: MyAlgebra[A]): Id[A] =
      fa match {
        case B(key) =>
          println(s"boolean($key)")
          key.asInstanceOf[A]
        case I(key) =>
          println(s"int($key)")
          key.asInstanceOf[A]
        case Or(a, b) =>
          println(s"($a or $b)")
          (a || b).asInstanceOf[A]
        case And(a, b) =>
          println(s"($a and $b)")
          (a && b).asInstanceOf[A]
        case Sum(a, b) =>
          println(s"($a + $b)")
          (a + b).asInstanceOf[A]
      }
  }

  println {
    "result: " + program.foldMap(compiler)
  }
}
