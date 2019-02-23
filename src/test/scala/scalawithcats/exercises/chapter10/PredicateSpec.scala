package scalawithcats.exercises.chapter10

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import Predicate._

class PredicateSpec extends FlatSpec with Matchers {

  private val isGreaterThanThree = Pure[List[String], Int] { op =>
    if (op > 3) Valid(op)
    else Invalid(List(s"Value '$op' is not greater than 3"))
  }

  private val isEven = Pure[List[String], Int] { op =>
    if (op % 2 == 0) Valid(op)
    else Invalid(List(s"Value '$op' is not an even number"))
  }

  private val isEvenAndGreaterThanThree = And(isEven, isGreaterThanThree)

  private val isEvenOrGreaterThanThree = Or(isEven, isEvenAndGreaterThanThree)

  behavior of "Pure"

  it should "return a Valid for a value that passes a given validation rule" in {
    isGreaterThanThree(4) shouldBe Valid(4)
  }

  it should "return an Invalid for a value that fails a given validation rule" in {
    isGreaterThanThree(3) shouldBe Invalid(List("Value '3' is not greater than 3"))
  }

  behavior of "And"

  it should "return a Valid for a value that passes both validation rules" in {
    isEvenAndGreaterThanThree(4) shouldBe Valid(4)
  }

  it should "return an Invalid for a value that fails the first validation rule" in {
    isEvenAndGreaterThanThree(5) shouldBe Invalid(List("Value '5' is not an even number"))
  }

  it should "return a Invalid for a value that fails the second validation rule" in {
    isEvenAndGreaterThanThree(2) shouldBe Invalid(List("Value '2' is not greater than 3"))
  }

  it should "return an Invalid containing both errors for a value that fails both validation rules" in {
    isEvenAndGreaterThanThree(3) shouldBe Invalid(List(
      "Value '3' is not an even number",
      "Value '3' is not greater than 3",
    ))
  }

  behavior of "Or"

  it should "return a Valid for a value that passes both validation rules" in {
    isEvenOrGreaterThanThree(4) shouldBe Valid(4)
  }

  it should "return a Valid for a value that passed only one validation rule" in {
    isEvenOrGreaterThanThree(2) shouldBe Valid(2)
  }

  it should "return an Invalid for a value that fails both validation rules" in {
    isEvenOrGreaterThanThree(3) shouldBe Invalid(List(
      "Value '3' is not an even number",
      "Value '3' is not greater than 3",
    ))
  }

  behavior of "run"

  it should "should return a function that returns an Either[E, A]" in {
    val f = isGreaterThanThree.run
    f(4) shouldBe Right(4)
  }

}
