package my_zio

import java.io.IOException

import zio._

object ZioTypes {

  type Task[+A] = ZIO[Any, Throwable, A]

  type UIO[+A] = ZIO[Any, Nothing, A]

  type RIO[-R, +A] = ZIO[R, Throwable, A]

  type IO[+E, +A] = ZIO[Any, E, A]

  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object HelloWorld extends ZIOAppDefault {


  def run: ZIO[Has[Console], IOException, Unit] =
    Console.printLine("Hello, World!")

  //  map() or as(): if we need to change success type:
  // (Console.printLine("hello") as 0) // or map(_ => 0)
}

object PrintSequence extends ZIOAppDefault {

  import Console._

  def run: ZIO[Has[Console], IOException, Unit] =
    printLine("hello") *> printLine("world")
}

object ErrorRecovery extends ZIOAppDefault {

  import Console._

  def failablePrint(s: String): ZIO[Has[Console], IOException, Unit] = printLine(s)

  def unfailablePrint(s: String): ZIO[Has[Console], Nothing, Unit] = printLine(s) orElse ZIO.succeed(()) // orElse() gets rid of error

  val failWithString: IO[String, Nothing] = ZIO.fail[String]("BOOM")

  def run: ZIO[Has[Console], IOException, Unit] = {

    val res: ZIO[Has[Console], String, Unit] =
      unfailablePrint("I am about to fail") *> // *> or zipRight()
        failWithString *>
        unfailablePrint("I will never be printed")


    // fold(): if I want to get rid of failure :
    val exitCode: URIO[Has[Console], Int] = res.fold(err => -1, success => 0)

    res.mapError((msg: String) => new IOException(msg)) // mapError: just change failure
  }

//teraz minuta 16
  //https://www.youtube.com/watch?v=TWdC7DhvD8M
}
