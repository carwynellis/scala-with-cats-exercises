package scalawithcats.exercises.chapter10

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._

class CheckSpec extends FlatSpec with Matchers {

  private val isGreaterThanThree = Pure[List[String], Int] { op =>
    if (op > 3) Right(op)
    else Left(List(s"Value '$op' is not greater than 3"))
  }

  private val isEven = Pure[List[String], Int] { op =>
    if (op % 2 == 0) Right(op)
    else Left(List(s"Value '$op' is not an even number"))
  }

  private val isEvenAndGreaterThanThree = And(isEven, isGreaterThanThree)

  behavior of "Pure"

  it should "return a Right(value) for a value that passes a given validation rule" in {
    isGreaterThanThree(4) shouldBe Right(4)
  }

  it should "return a Left(List(error)) for a value that fails a given validation rule" in {
    isGreaterThanThree(3) shouldBe Left(List("Value '3' is not greater than 3"))
  }

  behavior of "And"

  it should "return a Right(value) for a value that passes two validation rules" in {
    isEvenAndGreaterThanThree(4) shouldBe Right(4)
  }

  it should "return a Left(List(error)) for a value that fails the first validation rule" in {
    isEvenAndGreaterThanThree(5) shouldBe Left(List("Value '5' is not an even number"))
  }

  it should "return a Left(List(error)) for a value that fails the second validation rule" in {
    isEvenAndGreaterThanThree(2) shouldBe Left(List("Value '2' is not greater than 3"))
  }

  it should "return a Left with both errors for a value that fails both validation rules" in {
    isEvenAndGreaterThanThree(3) shouldBe Left(List(
      "Value '3' is not an even number",
      "Value '3' is not greater than 3",
    ))
  }

}
