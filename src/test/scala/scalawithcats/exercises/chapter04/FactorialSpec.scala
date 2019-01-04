package scalawithcats.exercises.chapter04

import cats.data.Writer
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class FactorialSpec extends FlatSpec with Matchers {

  behavior of "factorial"

  it should "compute the correct result and log the sequence of calculations" in {
    val (_, result) = Factorial.factorial(5).run
    result shouldBe (2 * 3 * 4 * 5)
  }

  it should "log the steps of the computation" in {
    val (log, _) = Factorial.factorial(2).run
    log shouldBe List(
      "fact 0 1",
      "fact 1 1",
      "fact 2 2"
    )
  }

  // This test just runs two factorialW invocations in parallel to demonstrate that the logging
  // is captured separately for each invocation. We could use AsyncFlatSpec here instead of using
  // Async.await...
  it should "log each invocation separately" in {
    val result = Await.result(Future.sequence(Vector(
      Future(Factorial.factorial(2)),
      Future(Factorial.factorial(3))
    )), 5 seconds)

    // This demonstrates that we have two writers with independent logs that could then be output
    // separately if required.
    result shouldBe Vector(
      Writer(List("fact 0 1", "fact 1 1", "fact 2 2"), 2),
      Writer(List("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6)
    )
  }

}
