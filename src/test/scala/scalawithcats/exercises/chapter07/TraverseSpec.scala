package scalawithcats.exercises.chapter07

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._
import cats.data.Validated.{Invalid, Valid}
import Traverse._

class TraverseSpec extends FlatSpec with Matchers {

  behavior of "listSequence"

  it should "transform a List containing two Vectors into a Vector of List with permutations of the input Lists" in {
    listSequence(List(Vector(1, 2), Vector(3, 4))) shouldBe Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))
  }

  it should "transform a List containing three Vectors into a Vector of List with permutations of the input Lists" in {
    listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6))) shouldBe
      Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))
  }

  behavior of "processOption"

  it should "return a Some of List containing the input List given a list of even numbers" in  {
    processOption(List(2, 4, 6)) shouldBe Some(List(2, 4, 6))
  }

  it should "return None given a List of even and odd numbers" in  {
    processOption(List(1, 2, 3)) shouldBe None
  }

  behavior of "processValidated"

  it should "return a Valid of list given a list of even numbers" in {
    processValidated(List(2, 4, 6)) shouldBe Valid(List(2, 4, 6))
  }

  it should "return an Invalid with error messages for each odd number given a list of even and odd numbers" in {
    processValidated(List(1, 2, 3)) shouldBe Invalid(List("1 is not even", "3 is not even"))
  }
}
