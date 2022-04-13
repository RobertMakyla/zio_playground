package my_zio

import java.io.IOException
import java.util.concurrent.TimeUnit

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

  def failablePrint(s: String): ZIO[Has[Console], IOException, Unit] = printLine(s)

  def unfailablePrint(s: String): ZIO[Has[Console], Nothing, Unit] = printLine(s) orElse ZIO.succeed(()) // orElse() - runs the second effect if the first one failed.
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
      _ <- printLine("what is you name ?")
      name <- readLine
      _ <- printLine(s"hello $name")
    } yield ()

}

object NumberGuesser extends ZIOAppDefault {

  import Console._
  import Random._

  def analyze(guess: String, answer: Int): ZIO[Has[Console], IOException, Unit] = {
    if (guess == answer.toString) printLine("OK :)") else printLine(s"Sorry, the answer was $answer")
  }

  def run: ZIO[ZEnv, IOException, Unit] = {
    for {
      randomInt <- nextIntBetween(1, 4) // from zio.Random
      _ <- printLine("Please write a number 0 to 3 ...")
      guess <- readLine
      _ <- analyze(guess, randomInt)
    } yield ()
  }

}

object AlarmDuration extends ZIOAppDefault {

  import Console._
  import java.io.IOException

  lazy val getAlarmDuration: ZIO[ZEnv, IOException, Duration] = {

    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO.attempt(input.toInt) // ZIO.attempt makes ZIO effect from risky code. Task = ZIO[Any, Throwable, A]
        .map(i => i.seconds)
        .refineToOrDie[NumberFormatException] // narrow the error type or make the fiber die !!!

    def fallback(input: String): ZIO[ZEnv, IOException, Duration] =
      printLine(s"You entered $input which is not valid for seconds, try again!") *> getAlarmDuration

    for {
      _ <- printLine("Please enter the number of seconds to sleep: ")
      input <- readLine
      duration <- parseDuration(input) orElse fallback(input) // orElse - runs the second effect if the first one fails
    } yield duration
  }

  def run: ZIO[ZEnv, IOException, Unit] = {
    for {
      duration <- getAlarmDuration
      //fiber <- (print(".") *> ZIO.sleep(1.seconds)).repeatN(duration.getSeconds.toInt) // just sleeping for n seconds and printing dots - no fork !
      //fiber <- (print(".") *> ZIO.sleep(1.seconds)).forever // fiber with effect repeating forever - the following code would never run - no fork !
      fiber <- (print(".") *> ZIO.sleep(1.seconds)).forever.fork // fork fiber with effect repeating forever - it runs in the background as a separate fiber
      _ <- ZIO.sleep(duration) // just sleeping for n seconds
      _ <- printLine("Time's up !")
      _ <- fiber.interrupt // elegant
    } yield ()
  }

}

object AsyncPrinting extends ZIOAppDefault {

  import Console._
  import Random._
  import Duration._
  import java.io.IOException

  def randomDuration: ZIO[ZEnv, Nothing, zio.Duration] =
    for {
      //millisInt <- nextIntBetween(100, 200)
      millisInt <- ZIO.succeed(100)
      duration <- ZIO.succeed(Duration.apply(millisInt, TimeUnit.MILLISECONDS))
    } yield duration

  def printingEffect(s: String): ZIO[ZEnv, IOException, Unit] =
    for {
      duration <- randomDuration
      _ <- print(s)
      _ <- ZIO.sleep(duration)
    } yield ()

  def run: ZIO[ZEnv, IOException, Unit] = {
    for {
      fiberA <- printingEffect("| ").forever.fork
      fiberB <- printingEffect("- ").forever.fork
      _ <- ZIO.sleep(10.seconds) // just sleeping for n seconds
      _ <- printLine("Time's up !")
      _ <- fiberA.interrupt
      _ <- fiberB.interrupt
    } yield ()
  }
}

object SyncPrinting extends ZIOAppDefault {

  import Console._
  import Duration._
  import java.io.IOException

  def keepPrintingA: ZIO[ZEnv, IOException, Unit] = print("a ") *> ZIO.sleep(100.millisecond)  *> keepPrintingB
  def keepPrintingB: ZIO[ZEnv, IOException, Unit] = print("b ") *> ZIO.sleep(100.millisecond)  *> keepPrintingA

  def run: ZIO[ZEnv, IOException, Unit] = keepPrintingA.forever
}