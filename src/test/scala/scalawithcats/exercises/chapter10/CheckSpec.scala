package scalawithcats.exercises.chapter10

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import Check._

class CheckSpec extends FlatSpec with Matchers {

  private val isGreaterThanThree = Predicate.Pure[List[String], Int] { op =>
    if (op > 3) Valid(op)
    else Invalid(List(s"Value '$op' is not greater than 3"))
  }

  behavior of "Pure"

  it should "return a Valid for a value that passes a given predicate" in {
    Check(isGreaterThanThree)(4) shouldBe Valid(4)
  }

  behavior of "Map"

  it should "return a Valid for a value that passes the given predicate containing the mapped result" in {
    Map(Pure(isGreaterThanThree), { i: Int => i + 1} )(4) shouldBe Valid(5)
  }

}
