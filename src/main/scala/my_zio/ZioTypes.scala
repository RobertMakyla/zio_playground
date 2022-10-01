package my_zio

import zio.Console.{ConsoleLive, printLine}

import java.io.IOException
import java.util.concurrent.TimeUnit
import zio._

import scala.annotation.tailrec

object ZioTypes {

  type Task[+A] = ZIO[Any, Throwable, A] // jak funkcja Javy

  type UIO[+A] = ZIO[Any, Nothing, A]

  type RIO[-R, +A] = ZIO[R, Throwable, A]

  type IO[+E, +A] = ZIO[Any, E, A] // wszystko może miec

  type URIO[-R, +A] = ZIO[R, Nothing, A]
}

object HelloWorld extends ZIOAppDefault {

  def run: ZIO[Any, IOException, Unit] = ZIO.service[Console].flatMap(_.printLine("Hello, ZIO")).provideEnvironment(ZEnvironment(ConsoleLive))
  //rule of thumb: import Console._ and then do printLine() - does exactly the same job, it's just a helper and it's easlier

  //  map(_ => 1) or as(): if we need to change success type:
  // (printLine("hello") as 0) // or map(_ => 0)
}

object PrintSequence extends ZIOAppDefault {

  import Console._

  def run: ZIO[Any, IOException, Unit] =
    printLine("1") *> printLine("2").flatMap(_ => printLine("3")) <* printLine("4")
    // '*>', or zipRight is also a flatMap that ignores first value.
    // However, zipLeft '<*' also sequences an effect but ignores the value produced by the second effect
}

object ErrorRecovery extends ZIOAppDefault {

  import Console._

  def unfaillableEither: ZIO[Any, Nothing, Either[Exception, Nothing]] = ZIO.fail(new Exception("boom")).either
  // ZIO.either changes possible error and result to unfailable Either[error, result)]
  // ZIO.absolve is oposite to either and turns an ZIO[R, Nothing, Either[E, A]] into a ZIO[R, E, A]:

  def failablePrint(s: String): ZIO[Console, IOException, Unit] = printLine(s)
  def unfailablePrint(s: String): ZIO[Console, Nothing, Unit] = printLine(s) orElse ZIO.unit // orElse() - try another effect if the first one fails.
  def unfailablePrintWithCatch(s: String): ZIO[Console, Nothing, Unit] = printLine(s).catchAll(e => unfailablePrint(e.getMessage)) // catchAll() catches error and returns second effect

  val failWithString: IO[String, Nothing] = ZIO.fail[String]("BOOM")

  override val run: ZIO[Any, Nothing, Unit] = {

    val res: ZIO[Console, String, Unit] =
      unfailablePrintWithCatch("I am about to fail") *> // *> is the same as zipRight()
        failWithString *>
        unfailablePrint("I will never be printed")


    /**
     * fold(): if I want to get rid of failure
     * it handles success and failure in noneffectual way
     *
     * foldZIO() - in an effectual way
     */
    val exitCode: URIO[Console, Int] = res.fold[Int](err => -1, success => 0)

    /**
     * foldZIO handles both success and failure in effectual way
     */
    val exitCodeFromEffects: ZIO[Console, Nothing, Int] = res.foldZIO(err => ZIO.succeed(-1), success => ZIO.succeed(1))

    val resChanged1: ZIO[Console, String, Int] = res.map(_ => 0) // map: just change success
    val resChanged1b: ZIO[Console, String, Int] = res as 0 // as === map: just change success
    val resChanged2: ZIO[Console, IOException, Unit] = res.mapError((msg: String) => new IOException(msg)) // mapError: just change failure

    ZIO.unit
  }
}

object Loops extends ZIOAppDefault {

  import Console._

  // Couldn't do it with Futures cause when you have a Future it's already running - no way to restart it.
  // ZIO - is completely lazy
  def loop[R, E, A](n: Int)(effect: ZIO[R, E, A]): ZIO[R, E, A] = {
    if (n <= 1) effect else effect *> loop(n - 1)(effect)
  }

  override def run: ZIO[Any, IOException, Unit] =
    loop(3)(Console.printLine("hello"))
}

object ProvidingEnvironment extends ZIOAppDefault {

  import Console._

  // ZIO.service[] -- access to environment
  // ZIO.serviceWith[Int](_.toString) -- gives way to run something on environment's function - the same as ZIO.service[Int].map(_.toString)
  def square: URIO[Int, Int] =
    for {
      env <- ZIO.service[Int]  // the same as:   ZIO.service[Int].map(env => env * env)
    } yield env * env

  def squareStr: URIO[Int, String] = ZIO.serviceWith[Int](env => (env * env).toString)

  def run: ZIO[Any, Throwable, Unit] = {
    for {
      sq <- square.provideEnvironment(ZEnvironment(4)) // ZEnvironment - provides Environment to an effect
      _ <- printLine("using service() : "+sq)
      sq2 <- squareStr.provideEnvironment(ZEnvironment(4)) // ZEnvironment - provides Environment to an effect
      _ <- printLine("using serviceWith() : " + sq2)
    } yield ()
  }
}

object PromptName extends ZIOAppDefault {

  import Console._

  //Implement something like:
  //
  //   printLine("what is you name ?") *>
  //   readLine *>
  //   printLine(s" hello $name")
  //
  def run: ZIO[Any, IOException, Unit] = forComprehension

  def classicWay: ZIO[Any, IOException, Unit] =
    printLine("what is you name ?") *> // '*>', or zipRight is also a flatMap that ignores first value. However, zipLeft '<*' also sequences an effect but ignores the value produced by the second effect
      readLine
        .flatMap { // flatMap to get a value and use it
          name =>
            printLine(s" hello $name")
        }

  def forComprehension: ZIO[Any, IOException, Unit] =
    for {
      _ <- printLine("what is you name ?")
      name <- readLine
      _ <- printLine(s"hello $name")
    } yield ()

}

/**
 * instead of low-level Fibers we should use high-lever parallel operations
 */
object ParallelOperations extends ZIOAppDefault {

  import Console._
  import Random._


  def run: ZIO[Any, IOException, Unit] = {
    for {
      _ <- printLine("I go first").zip(printLine(", I go second")) // zipping 2 effects into one (sequentially), making a tuple of results if there are any
      _ <- printLine("am I first ?").zipPar(printLine(" or maybe I am first ?")) // zipping 2 effects into one (parallel)

      chunkOfEffects = NonEmptyChunk(ZIO.succeed(1), ZIO.succeed(2), ZIO.succeed("abc"))
      chunkOfValues = NonEmptyChunk(1, 2, "abc")

      res1 <- ZIO.collectAllPar(chunkOfEffects) // calculating in parallel, returning in the right order
      _ <- printLine("collectAllPar: " + res1)

      res2 <- ZIO.foreachPar(chunkOfValues)(x => ZIO.succeed(x.toString.toUpperCase)) // applying function in parallel, returning in the right order
      _ <- printLine("foreachPar: " + res2)

      res3 <- ZIO.reduceAllPar(ZIO.succeed(0), chunkOfEffects)((a,b) => a.toString + b.toString) // reduces effects into 1 effect, in parallel
      _ <- printLine("reduceAllPar: " + res3)

      res4 <- ZIO.mergeAllPar(chunkOfEffects)("0")((a,b) => a + b.toString) // reduces effects into 1 effect, in parallel
      _ <- printLine("mergeAllPar: " + res4)

      winner <- ZIO.succeed("first").race(ZIO.succeed("second"))
      _ <- printLine("race: " + winner)

      res5 <- ZIO.succeed("Hello").timeout(1.seconds) // timeout changes T into Option[T]
      _ <- printLine(res5)
    } yield ()
  }

}


object NumberGuesser extends ZIOAppDefault {

  import Console._
  import Random._

  def tryToGuess(correctNumber: Int): ZIO[Any, IOException, Unit] =
    for {
      _ <- printLine("Please write a number 1 to 10 ...")
      guess <- readLine
      _ <- {
        if (guess > correctNumber.toString) printLine("try smaller") *> tryToGuess(correctNumber)
        else if (guess < correctNumber.toString) printLine("try bigger") *> tryToGuess(correctNumber)
        else printLine("You won !!! :)")
      }
    } yield ()

  def run: ZIO[Any, IOException, Unit] = {
    for {
      randomInt <- nextIntBetween(1, 11) // from zio.Random
      _ <- tryToGuess(randomInt)
    } yield ()
  }

}

/**
 * TO REMEMBER: When we have some code/effect which is blocking the thread/fiber, enclose it in ZIO.blocking ...
 * this will
 */
object BlockingSideEffects extends ZIOAppDefault {
  import Console._
  import Duration._
  override def run: ZIO[Any, Throwable, Unit] = {
    printLine("hello") *>
    ZIO.attemptBlocking(Thread.sleep(1000)) *> // ZIO.attemptBlocking( nonEffectual  ) - makes sure it's executed in a separate thread pool for potential blockers.
    printLine("1 sec just passed") *>
    ZIO.blocking(ZIO.sleep(1.seconds) *> printLine("... and another one")) // ZIO.blocking( effectual )   - the same but it takes effectual argument
  }
}

object SeparatingBlockingEffectButNotInterrupting extends ZIOAppDefault {

  import Console._
  import Duration._

  def run =
    for {
      fiber <- ZIO.attemptBlocking { // REMEMBER: when we interrupt ZIO.attemptBlocking{} it will still NOT STOP. It will stop when JVM stops !!!
                while (true) {
                  Thread.sleep(200)
                  println("loop")
                }
              }.ensuring(printLine("End of a blocking operation - WILL NEVER BE PRINTED").orDie) // or I can use: onInterrupt()
               .fork
      _ <- ZIO.sleep(1.seconds)
      _ <- fiber.interrupt // it will not interrupt the underlying loop
    } yield ()

}

/**
 * Instead of interrupting a fiber I can always call .timeout(Duration) on any effect. It returns Some() if we have result before deadline or None if  we don't.
 */
object InterruptingBlockingEffect extends ZIOAppDefault {

  import Console._
  import Duration._

  def run =
    for {
      _     <- printLine("Start blocking operation")
      fiber <- ZIO.attemptBlockingInterrupt{ // REMEMBER: this can be interrupted :)
                while (true) {
                  Thread.sleep(200)
                  println("loop")
                }
              }.onInterrupt(printLine("End of a blocking operation").orDie)
               .fork
      _ <- ZIO.sleep(1.seconds)
      _ <- fiber.interrupt
    } yield ()

}

/**
 * FiberRef[T] - it’s exactly like Ref[A] (mutable reference) but in the scope of the fiber.
 * So when I fork a child fiber, the initial FiberRef[T] values are equal to parent’s values
 * (copy-on-fork)
 */
object FiberRefTest extends ZIOAppDefault {

  import Console._

  def newFiberForked[A](fr: FiberRef[A])(incr: A => A) = (
    for {
      _ <- fr.update(incr)
      _ <- fr.get.flatMap(nv => printLine("new value: " + nv))
    } yield ()
    ).fork

  def run =
    for {
      fr <- FiberRef.make[Int](1)
      f1 <- newFiberForked[Int](fr)(i => i * 10)
      f2 <- newFiberForked[Int](fr)(i => i * 20)
      oldFr <- fr.get
      _ <- printLine("old value: " + oldFr)
      _ <- f1.interrupt
      _ <- f2.interrupt
    } yield ()
}

/**
 * We always have CORE concerns (eg: calculate business data) and CROSS-CUTTING concerns (retry, logging, etc)
 *
 * Aspect-oriented programming - help us increase modularity by SEPARATION OF CONCERNS.
 * It does it by adding behavior to existing code without modifying it.
 * With ASPECTS, the cross-cutting (not core/not business) logic can be added without modifying the code (business) logic
 *
 * 1. computing business data (fail or pass)
 * 2. retrying
 * 3. logging the data
 *
 * ZIO Aspect is like a function ZIO[R, E, A] => ZIO[R, E, A] (not impacting the types)
 */
object Aspects extends ZIOAppDefault {

  import Console._
  import Duration._
  import Random._

  def intPassOrFail(passPercentage: Int) = {
    nextIntBetween(0, 100)
      .flatMap(random => if (random < passPercentage) ZIO.succeed("hello") else ZIO.fail(new IOException("boom")))
  }

  override def run =
    intPassOrFail(50) @@
      ZIOAspect.retry(Schedule.fibonacci(100.milliseconds)) @@
      ZIOAspect.logged("Successful computation") @@
      ZIOAspect.loggedWith[String](msg => s"Final result: ${msg}")

}

/**
 * fork - from ZIO[R, E, A] it makes URIO[R, Fiber[E, A]]
 * forkDeamon - makes a new global fiber, so if the main fiber ends, this one will still be running
 */
object HelloFibers extends ZIOAppDefault {

  import Console._

  def run: ZIO[Any, IOException, Unit] = for {
    _          <- printLine("get up")                // fiber no.1 (main)
    fiber1     <- printLine("brush your teeth").fork // fiber no.2
    fiber2     <- printLine("get dressed").fork      // fiber no.3
    fiberReady = fiber1.zip(fiber2) // sequential execution, making a tuple of results. nice alias for zipping: <*>
    _          <- fiberReady.join // joins wait for fiber 1 and 2 (zipped together) to be complete before we continue
    _          <- printLine("ready to go")
  } yield ()
}

object ParallelCalculation extends ZIOAppDefault {

  import Console._

  def run: ZIO[Any, IOException, Unit] = for {
    fiber1 <- (printLine("calculating 1 in parallel") *> ZIO.succeed(1)).fork // Returns an effect that forks this effect into its own separate fiber, ...
    fiber2 <- (printLine("calculating 2 in parallel") *> ZIO.succeed(2)).fork // ... returning the fiber immediately, without waiting for it to begin executing the effect.
    a <- fiber1.join // suspends the main fiber until the result of joining fiber has been determined.
    b <- fiber2.join
    _ <- printLine("Sum: " + (a + b))
  } yield ()
}

object LoopWithZIO extends ZIOAppDefault {

  def loopWithExplicitRecursion(from: Int, to: Int): ZIO[Any, IOException, Unit] = {
    if (from <= to) Console.printLine(s"$from") *> loopWithExplicitRecursion(from + 1, to) else ZIO.unit
  }

  def loopZIO(from: Int, to: Int) =
    ZIO.loop(from)(_ <= to, _ + 1)(i => Console.printLine(s"$i")) // ZIO.loopDiscard() works the same but result is Unit as it discards all the result

  def run = loopWithExplicitRecursion(1, 3) *> loopZIO(4, 6)
}

object AlarmDuration extends ZIOAppDefault {

  import Console._
  import java.io.IOException

  lazy val getAlarmDuration: ZIO[Any, IOException, Duration] = {

    def parseDuration(input: String): IO[NumberFormatException, Duration] =
      ZIO.attempt(input.toInt) // ZIO.attempt makes ZIO effect from risky code. Task = ZIO[Any, Throwable, A]
        .map(i => i.seconds)
        .refineToOrDie[NumberFormatException] // narrow the error type or make the fiber die !!!

    def myFallback(input: String): ZIO[Any, IOException, Duration] =
      printLine(s"You entered $input which is not valid for seconds, try again!") *> getAlarmDuration

    for {
      _ <- printLine("Please enter the number of seconds to sleep: ")
      input <- readLine
      duration <- parseDuration(input) orElse myFallback(input) // orElse - runs the second effect if the first one fails
    } yield duration
  }

  def run: ZIO[Any, IOException, Unit] = {
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

  def run: ZIO[Any, IOException, Unit] = {
    val effect1 = print("o ") *> ZIO.sleep(100.millisecond)
    val effect2 = print("| ") *> ZIO.sleep(100.millisecond)

    for {
      fiber <- effect1.race(effect2).forever.fork // more elegant way than using low-level fibers
      //      fiberA <- effect1.forever.fork // it forks this effect into its own separate fiber,
      //      fiberB <- effect2.forever.fork
      _ <- ZIO.sleep(3.seconds) // just sleeping for n seconds
      _ <- printLine("Time's up !")
      _ <- fiber.interrupt
//      _ <- fiberA.interrupt
//      _ <- fiberB.interrupt
    } yield ()
  }
}

object SyncPrinting extends ZIOAppDefault {

  import Console._
  import Duration._
  import java.io.IOException

  def keepPrintingA: ZIO[Any, IOException, Unit] = print("o ") *> ZIO.sleep(100.millisecond)  *> keepPrintingB
  def keepPrintingB: ZIO[Any, IOException, Unit] = print("| ") *> ZIO.sleep(100.millisecond)  *> keepPrintingA

  def run: ZIO[Any, IOException, Unit] = keepPrintingA.forever
}

/**
 * Ref - set/get - mutable reference to immutable data (atomic reference)
 * Ref.Synchronized (former RefM) - has the same methods but allows to calculate effects within update() and use the effect's result to update the value.
 *                                 Ref.Synchronized.modifyZIO(state -> effect)
 *                                 during calculations all the Writes are blocked, Reads are permitted
 */
object SyncPrintingRef extends ZIOAppDefault {

  import Console._
  import Duration._
  import java.io.IOException

  def printSyncOnRef(expectedRef: Boolean, ref: Ref[Boolean], s: String): ZIO[Any, IOException, Unit] =
    for {
      actualRef <- ref.get
//      _ <- if (actualRef == expectedRef) print(s) *> ref.update(!_) else ZIO.unit
      _ <-ZIO.ifZIO(ZIO.succeed(actualRef == expectedRef))(onTrue = print(s) *> ref.update(!_), onFalse =  ZIO.unit)
    _ <- ZIO.sleep(50.millisecond)
    } yield ()

  def run: ZIO[Any, IOException, Unit] = {
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
   * and releasing it immediately after the effect completes execution.
   *
   * When the acquire operation cannot be performed, due to insufficient permits,
   * such task is placed in internal suspended fibers queue and will be awaken when permits value is sufficient:
   */
  def printOnSemaphore(sem: Semaphore, s: String): ZIO[Any, IOException, Unit] =
    sem.withPermit(print(s))

  def run: ZIO[Any, IOException, Unit] = {
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

/**
 * Promise can be used when we want to wait for sth to happen (1 fiber can wait for another one)
 */
object PromiseExample extends ZIOAppDefault {

  import Console._
  import Duration._

  def run: ZIO[Any, IOException, Unit] =
    for {
      promise <- Promise.make[Nothing, String] // create a promise
          sendString = (ZIO.succeed("hello world") <* ZIO.sleep(2.second)).flatMap(s => promise.succeed(s)) // complete the promise after 1 sec
          getAndPrint = promise.await.flatMap(s => printLine(s)) // await for the promise and print the result
      fiberA <- sendString.fork
      fiberB <- getAndPrint.fork
      _ <- (fiberA <*> fiberB).join
    } yield ()
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

  def randomTuple: ZIO[Any, Nothing, (Double, Double)] = nextDouble zip nextDouble // zip() is making a tuple

  def isInsideCircle(x: Double, y: Double): Boolean = Math.sqrt(x * x + y * y) <= 1

  /** Ref is a wrapper for variables, lazy and composable with ZIO effects
   Ref can be read/updated 100% the isolated way - atomically - without using any locking !*/
  def increaseStateNumbers(ref: Ref[PointsState]): ZIO[Any, Nothing, Unit] =
    for {
      tuple <- randomTuple
      (x, y) = tuple
      inside = if (isInsideCircle(x, y)) 1 else 0
      _ <- ref.update(state => PointsState(state.inTheCircle + inside, state.total + 1)) // update: Atomically modifies the `ZRef`
    } yield ()


  def printCurrentPi(ref: Ref[PointsState]) =
    for {
      state <- ref.get
      _ <- printLine("" + estimatePi(state.inTheCircle, state.total))
    } yield ()

  def run: ZIO[Any, IOException, Unit] =
    for {
      ref     <- Ref.make(PointsState(0L, 0L)) // making a new Ref
      worker: ZIO[Any, IOException, Unit]   = increaseStateNumbers(ref).forever // "=" means that we are not putting it yet into ZIO effects
      workers: List[ZIO[Any, IOException, Unit]] = List.fill(10)(worker) // List of ZIO effects
      fiber1   <- ZIO.forkAll(workers) // forking a list of workers into one separate fiber
      fiber2   <- (printCurrentPi(ref) *> ZIO.sleep(1.second)).forever.fork
      _        <- ZIO.sleep(30.seconds)
      _        <- fiber1.interrupt
      _        <- fiber2.interrupt
    } yield ()
}

object ComputePiApp extends ZIOAppDefault {
  def run: ZIO[Any, IOException, Unit] = (new ComputePi).run
}


/**
 * -- Dining Philosophers --
 *
 * STM = Software Transaction Memory solution - solving concurrency problem (race condition/deadlocks) without using locks/
 */

//class SmtDiningPhilosophers {
//  import stm._
//  import Console._
//
//  class Fork
//
//  /**
//   * Ref  - isolated operation in the context of the Ref
//   * TRef - isolated operations in the context of a transaction !!
//   *        I can modify many TRefs in a single transaction.
//   *
//   * STM[Failure, Success] - transaction type
//   *                        it can compose with map/flatMap/zipRight
//   */
//  case class Placement(left: TRef[Option[Fork]], right: TRef[Option[Fork]]) // TRef comes from zio.smt
//
//  case class Roundtable(seats: List[Placement])
//
//  /**
//   * STM = transaction on getting the 2 forks (no need to use locks around both forks)
//   * without STM we would need to do it on some order :/ eg: lock f1 and check if it's available before we get f2:/
//   */
//  def takeForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]]): STM[Nothing, (Fork, Fork)] =
//    for {
//      leftFork <- left.get.collect { case Some(fork) => fork } // if the value does not match the PartialFunction, STM makes it retry until it does !
//      rightFork <- right.get.collect { case Some(fork) => fork } // it's complete when both forks are available !
//      _ <- left.set(None)
//      _ <- right.set(None)
//    } yield (leftFork, rightFork)
//
//  /**
//   * STM = transaction on putting the 2 forks
//   */
//  def putForks(left: TRef[Option[Fork]], right: TRef[Option[Fork]])(tuple: (Fork, Fork)): STM[Nothing, Unit] = {
//    val (l, r) = tuple
//    for{
//      _ <- right.set(Some(r))
//      _ <- left.set(Some(l))
//    } yield ()
//  }
//
//  def setupTable(size: Int): ZIO[Any, Nothing, Roundtable] = {
//    def makeFork: USTM[TRef[Option[Fork]]]= TRef.make[Option[Fork]](Some(new Fork)) //TRef.make gives you an STM
//
//    (
//      for {
//        allForks0: Seq[TRef[Option[Fork]]] <- STM.foreach(0 to size)(i => makeFork) //STM.foreach(list)(_ => STM[A])  gives   STM[Seq[A]]
//        allForks = allForks0 ++ List(allForks0(0))
//        placements = (allForks zip allForks.drop(1)).map {
//          case (l, r) => Placement(l, r)
//        }
//      } yield Roundtable(placements.toList)
//     ).commit // you need to call STM.commit in order to get ZIO effect
//  }
//
//  def eat(philosopher: Int, roundtable: Roundtable): ZIO[Any, IOException, Unit] = {
//    val placement = roundtable.seats(philosopher)
//
//    val leftFork  = placement.left
//    val rightFork = placement.right
//
//    for {
//      forks <- takeForks(leftFork, rightFork).commit
//      _     <- printLine(s"Philosopher ${philosopher} eating...")
//      _     <- putForks(leftFork, rightFork)(forks).commit
//      _     <- printLine(s"Philosopher ${philosopher} is done eating")
//    } yield ()
//  }
//
//
//  def run: ZIO[Any, IOException, Unit] = {
//    val count = 10
//
//    def eaters(table: Roundtable): Iterable[ZIO[Any, IOException, Unit]] =
//      (0 to count).map(i => eat(i, table))
//
//    for {
//      table <- setupTable(count)
//      fiber <- ZIO.forkAll(eaters(table)) // forking a list of ZIOs into 1 separate fiber
//      _     <- fiber.join
//      _     <- printLine("All philosophers have eaten!")
//    } yield ()
//  }
//
//}
//
//object SmtDiningPhilosophersApp extends ZIOAppDefault {
//  def run: ZIO[Any, IOException, Unit] = (new SmtDiningPhilosophers).run
//}