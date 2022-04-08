package my_zio

import java.io.IOException

import zio._

object ZioTypes {

  type Task[+A] = ZIO[Any, Throwable, A] // jak funkcja Javy

  type UIO[+A] = ZIO[Any, Nothing, A]

  type RIO[-R, +A] = ZIO[R, Throwable, A]

  type IO[+E, +A] = ZIO[Any, E, A] // wszystko może miec

  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object HelloWorld extends ZIOAppDefault {

  import Console._
  def run: ZIO[Has[Console], IOException, Unit] = printLine("Hello, World!")

  //  map(_ => 1) or as(): if we need to change success type:
  // (printLine("hello") as 0) // or map(_ => 0)
}

object PrintSequence extends ZIOAppDefault {

  import Console._

  def run: ZIO[Has[Console], IOException, Unit] =
    printLine("hello") *> printLine("world")
}

object ErrorRecovery extends ZIOAppDefault {

  import Console._

  def failablePrint(s: String):            ZIO[Has[Console], IOException, Unit] = printLine(s)
  def unfailablePrint(s: String):          ZIO[Has[Console], Nothing, Unit] = printLine(s) orElse ZIO.succeed(()) // orElse() - runs the second effect if the first one failed.
  def unfailablePrintWithCatch(s: String): ZIO[Has[Console], Nothing, Unit] = printLine(s).catchAllCause(cause => unfailablePrint(cause.prettyPrint)) // catchAll[Cause] catches error and returns second effect

  val failWithString: IO[String, Nothing] = ZIO.fail[String]("BOOM")

  def run: ZIO[Has[Console], IOException, Unit] = {

    val res: ZIO[Has[Console], String, Unit] =
      unfailablePrintWithCatch("I am about to fail") *> // *> is the same as zipRight()
        failWithString *>
        unfailablePrint("I will never be printed")


    // fold(): if I want to get rid of failure :
    val exitCode: URIO[Has[Console], Int] = res.fold(err => -1, success => 0)

    res.mapError((msg: String) => new IOException(msg)) // mapError: just change failure
  }
}

object Loops extends ZIOAppDefault {

  import Console._

  // Couldn't do it with Futures cause when you have a Future it's already running - no way to restart it.
  // ZIO - is completely lazy
  def loop[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, A] = {
    if (n <= 0) effect else effect *> loop(n - 1)(effect)
  }

  def run: ZIO[Has[Console], IOException, Unit] =
    loop(10)(printLine("hello"))
}

object PromptName extends ZIOAppDefault {

  import Console._

  //Implement something like:
  //
  //   printLine("what is you name ?") *>
  //   readLine *>
  //   printLine(s" hello $name")
  //
  def run: ZIO[Has[Console], IOException, Unit] = forComprehension

  def classicWay: ZIO[Has[Console], IOException, Unit] =
    printLine("what is you name ?") *> // zipRight is also a flatMap
      readLine
        .flatMap { // flatMap to get a value and use it
          name =>
            printLine(s" hello $name")
        }

  def forComprehension: ZIO[Has[Console], IOException, Unit] =
    for {
      _    <- printLine("what is you name ?")
      name <- readLine
      _    <- printLine(s"hello $name")
    } yield ()

}

