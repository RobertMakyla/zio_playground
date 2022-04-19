package my_zio

import java.io.IOException
import java.util.concurrent.TimeUnit

import zio.{ZEnv, _}

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

object HelloFibers extends ZIOAppDefault {

  import Console._

  def run: ZIO[ZEnv, IOException, Unit] = for {
    _          <- printLine("get up")
    fiber1     <- printLine("brush your teeth").fork
    fiber2     <- printLine("get dressed").fork
    fiberReady = fiber1.zip(fiber2) // nice alias for zipping: <*>
    _          <- fiberReady.join // joins wait for fiber 1 and 2 (zipped together) to be complete before we continue
    _          <- printLine("ready to go")
  } yield ()
}

object ParallelCalculation extends ZIOAppDefault {

  import Console._

  def run: ZIO[ZEnv, IOException, Unit] = for {
    fiber1 <- (printLine("calculating 1 in parallel") *> ZIO.succeed(1)).fork // Returns an effect that forks this effect into its own separate fiber, ...
    fiber2 <- (printLine("calculating 2 in parallel") *> ZIO.succeed(2)).fork // ... returning the fiber immediately, without waiting for it to begin executing the effect.
    a <- fiber1.join // suspends the main fiber until the result of joining fiber has been determined.
    b <- fiber2.join
    _ <- printLine("Sum: " + (a + b))
  } yield ()
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
      fiber <- (print(".") *> ZIO.sleep(1.seconds)).forever.fork // it forks this effect into its own separate fiber,
      _ <- ZIO.sleep(duration) // just sleeping for n seconds
      _ <- printLine("Time's up !")
      _ <- fiber.interrupt // elegant
    } yield ()
  }

}

object AsyncPrinting extends ZIOAppDefault {

  import Console._
  import Duration._
  import java.io.IOException

  def run: ZIO[ZEnv, IOException, Unit] = {
    for {
      fiberA <- (print("o ") *> ZIO.sleep(100.millisecond)).forever.fork // it forks this effect into its own separate fiber,
      fiberB <- (print("| ") *> ZIO.sleep(100.millisecond)).forever.fork
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

  def keepPrintingA: ZIO[ZEnv, IOException, Unit] = print("o ") *> ZIO.sleep(100.millisecond)  *> keepPrintingB
  def keepPrintingB: ZIO[ZEnv, IOException, Unit] = print("| ") *> ZIO.sleep(100.millisecond)  *> keepPrintingA

  def run: ZIO[ZEnv, IOException, Unit] = keepPrintingA.forever
}

object SyncPrintingRef extends ZIOAppDefault {

  import Console._
  import Duration._
  import java.io.IOException

  def printSyncOnRef(expectedRef: Boolean, ref: Ref[Boolean], s: String): ZIO[ZEnv, IOException, Unit] =
    for {
      actualRef <- ref.get
      _ <- if (actualRef == expectedRef) print(s) *> ref.update(!_) else ZIO.succeed(())
//    _ <- ZIO.sleep(100.millisecond)
    } yield ()

  def run: ZIO[ZEnv, IOException, Unit] = {
    for {
      ref <- Ref.make(true)
      fiberA <- printSyncOnRef(true, ref, "o ").forever.fork // it forks this effect into its own separate fiber,
      fiberB <- printSyncOnRef(false, ref, "| ").forever.fork // it forks this effect into its own separate fiber,
      _ <- ZIO.sleep(1.seconds)
      _ <- printLine("Time's up !")
      _ <- fiberA.interrupt
      _ <- fiberB.interrupt
    } yield ()
  }
}

object SemaphoreExample extends ZIOAppDefault {

  import Console._
  import java.io.IOException

  /**
   * Semaphore.withPermit() -
   *
   * Executes the specified effect, acquiring a permit
   * immediately before the effect begins execution
   * and releasing it immediately after the effect completes execution,
   */
  def printOnSemaphore(sem: Semaphore, s: String): ZIO[ZEnv, IOException, Unit] =
    sem.withPermit(print(s))

  def run: ZIO[ZEnv, IOException, Unit] = {
    for {
      singlePermitSemaphore <- Semaphore.make(1) // Making a semaphore with 1 permit
      fiberA <- printOnSemaphore(singlePermitSemaphore, "o ").forever.fork
      fiberB <- printOnSemaphore(singlePermitSemaphore, "| ").forever.fork
      _ <- ZIO.sleep(100.millisecond)
      _ <- printLine("Semaphore can only make sure that calculation is in progress by maximum 1 fiber - but task order is still pretty random")
      _ <- fiberA.interrupt
      _ <- fiberB.interrupt
    } yield ()
  }
}

/*
 * Build a multi-fiber program that estimates the value of `pi`. Print out ongoing estimates continuously until the estimation is complete.
 *
 * Computing Pi with Monte Carlo method
 *
 * |------|
 * | (  ) |
 * |------|
 *
 * Pi = (points inside circle / total points) * 4
 * And the more random points we get the more correct Pi we calculate
 *
 * info: https://www.101computing.net/estimating-pi-using-the-monte-carlo-method/
 */

class ComputePi  {

  import Random._
  import Console._
  import Duration._

  final case class PointsState(inTheCircle: Long, total: Long)

  // Pi = (points inside circle / points inside square) *4
  def estimatePi(inside: Long, total: Long): Double = (inside.toDouble / total.toDouble) * 4

  def randomTuple: ZIO[ZEnv, Nothing, (Double, Double)] = nextDouble zip nextDouble // zip() is making a tuple

  def isInsideCircle(x: Double, y: Double): Boolean = Math.sqrt(x * x + y * y) <= 1

  /** Ref is a wrapper for variables, lazy and composable with ZIO effects
   Ref can be read/updated 100% the isolated way - atomically - without using any locking !*/
  def increaseStateNumbers(ref: Ref[PointsState]): ZIO[ZEnv, IOException, Unit] = (
      for {
        tuple: (Double, Double) <- randomTuple
        (x, y)                  = tuple
        inside                  = if (isInsideCircle(x, y)) 1 else 0
        _                       <- ref.update(state => PointsState(state.inTheCircle + inside, state.total + 1)) // update: Atomically modifies the `ZRef`
      } yield ()
    ).orElse(ZIO.fail(new IOException("increase failed")))

  def printCurrentPi(ref: Ref[PointsState]) =
    for {
      state <- ref.get
      _ <- printLine("" + estimatePi(state.inTheCircle, state.total))
    } yield ()

  def run: ZIO[ZEnv, IOException, Unit] =
    for {
      ref     <- Ref.make(PointsState(0L, 0L)) // making a new Ref
      worker: ZIO[ZEnv, IOException, Unit]   = increaseStateNumbers(ref).forever // "=" means that we are not putting it yet into ZIO effects
      workers: List[ZIO[ZEnv, IOException, Unit]] = List.fill(10)(worker) // List of ZIO effects
      fiber1   <- ZIO.forkAll(workers) // forking a list of workers into one separate fiber
      fiber2   <- (printCurrentPi(ref) *> ZIO.sleep(1.second)).forever.fork
      _        <- ZIO.sleep(30.seconds)
      _        <- fiber1.interrupt
      _        <- fiber2.interrupt
    } yield ()
}

object ComputePiApp extends ZIOAppDefault {
  def run: ZIO[ZEnv, IOException, Unit] = (new ComputePi).run
}


/**
 * -- Dining Philosophers --
 *
 * STM = Software Transaction Memory solution - solving concurrency problem (race condition/deadlocks) without using locks/
 */
class SmtDiningPhilosophers {
  import stm._
  import Console._

  class Fork

  /**
   * Ref  - isolated operation in the context of the Ref
   * TRef - isolated operations in the context of a transaction !!
   *        I can modify many TRefs in a single transaction.
   *
   * STM[Failure, Success] - transaction type
   *                        it can compose with map/flatMap/zipRight
   */
  case class Placement(left: TRef[Option[Fork]], right: TRef[Option[Fork]]) // TRef comes from zio.smt

  case class Roundtable(seats: List[Placement])

  /**
   * STM = transaction on getting the 2 forks (no need to use locks around both forks)
   * without STM we would need to do it on some order :/ eg: lock f1 and check if it's available before we get f2:/
   */
  def takeForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]]): STM[Nothing, (Fork, Fork)] =
    for {
      leftFork <- left.get.collect { case Some(fork) => fork } // if the value does not match the PartialFunction, STM makes it retry until it does !
      rightFork <- right.get.collect { case Some(fork) => fork } // it's complete when both forks are available !
      _ <- left.set(None)
      _ <- right.set(None)
    } yield (leftFork, rightFork)

  /**
   * STM = transaction on putting the 2 forks
   */
  def putForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]])(tuple: (Fork, Fork)): STM[Nothing, Unit] = {
    val (l, r) = tuple
    for{
      _ <- right.set(Some(r))
      _ <- left.set(Some(l))
    } yield ()
  }

  def setupTable(size: Int): ZIO[Any, Nothing, Roundtable] = {
    def makeFork: USTM[TRef[Option[Fork]]]= TRef.make[Option[Fork]](Some(new Fork)) //TRef.make gives you an STM

    (
      for {
        allForks0: Seq[TRef[Option[Fork]]] <- STM.foreach(0 to size)(i => makeFork) //STM.foreach(list)(_ => STM[A])  gives   STM[Seq[A]]
        allForks = allForks0 ++ List(allForks0(0))
        placements = (allForks zip allForks.drop(1)).map {
          case (l, r) => Placement(l, r)
        }
      } yield Roundtable(placements.toList)
     ).commit // you need to call STM.commit in order to get ZIO effect
  }

  def eat(philosopher: Int, roundtable: Roundtable): ZIO[Has[Console], IOException, Unit] = {
    val placement = roundtable.seats(philosopher)

    val leftFork  = placement.left
    val rightFork = placement.right

    for {
      forks <- takeForks(leftFork, rightFork).commit
      _     <- printLine(s"Philosopher ${philosopher} eating...")
      _     <- putForks(leftFork, rightFork)(forks).commit
      _     <- printLine(s"Philosopher ${philosopher} is done eating")
    } yield ()
  }


  def run: ZIO[ZEnv, IOException, Unit] = {
    val count = 10

    def eaters(table: Roundtable): Iterable[ZIO[Has[Console], IOException, Unit]] =
      (0 to count).map(i => eat(i, table))

    for {
      table <- setupTable(count)
      fiber <- ZIO.forkAll(eaters(table)) // forking a list of ZIOs into 1 separate fiber
      _     <- fiber.join
      _     <- printLine("All philosophers have eaten!")
    } yield ()
  }

}

object SmtDiningPhilosophersApp extends ZIOAppDefault {
  def run: ZIO[ZEnv, IOException, Unit] = (new SmtDiningPhilosophers).run
}