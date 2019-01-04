package scalawithcats.exercises.chapter04

import cats.data.Writer
import org.scalatest.{AsyncFlatSpec, Matchers}
import scala.concurrent.Future

class FactorialSpec extends AsyncFlatSpec with Matchers {

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
  // is captured separately for each invocation.
  it should "log each invocation separately" in {
    Future.sequence(Vector(
      Future(Factorial.factorial(2)),
      Future(Factorial.factorial(3))
    )).map { result =>
      // This demonstrates that we have two writers with independent logs that could then be output
      // separately if required.
      result shouldBe Vector(
        Writer(List("fact 0 1", "fact 1 1", "fact 2 2"), 2),
        Writer(List("fact 0 1", "fact 1 1", "fact 2 2", "fact 3 6"), 6)
      )
    }
  }


}
