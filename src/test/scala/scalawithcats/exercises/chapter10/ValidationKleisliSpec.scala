package scalawithcats.exercises.chapter10

import cats.data.Validated.{Invalid, Valid}
import org.scalatest.{FlatSpec, Matchers}
import scalawithcats.exercises.chapter10.ValidationKleisli._

class ValidationKleisliSpec extends FlatSpec with Matchers {

  behavior of "validateUsername"

  it should "accept a valid username" in {
    validateUsername("ValidUsername") shouldBe Right("ValidUsername")
  }

  it should "reject a username that is too short" in {
    validateUsername("Foo") shouldBe Left(error("Must be longer than 3 characters"))
  }

  it should "reject a username that does not contain only alphanumeric characters" in {
    validateUsername("FooBar!") shouldBe Left(error("Must be all alphanumeric characters"))
  }

  behavior of "validateEmailAddress"

  ignore should "accept a valid email address" in {
    validateEmailAddress("foo@example.com") shouldBe Valid("foo@example.com")
  }

  ignore should "reject an email address that does not contain an @ symbol" in {
    validateEmailAddress("example.com") shouldBe Invalid(error("Must contain the character @"))
  }

  ignore should "reject an email address that contains no characters before the @ sign" in {
    validateEmailAddress("@example.com") shouldBe Invalid(error("Must contain characters before the @ sign"))
  }

  ignore should "reject an email address that contains less than three characters after the @ sign" in {
    validateEmailAddress("foo@e.") shouldBe Invalid(error("Must contain at least 3 characters after the @ sign"))
  }

  ignore should "reject an email address that does not contain a . in the string following the @ sign" in {
    validateEmailAddress("foo@examplecom") shouldBe Invalid(error("Must contain a . after the @ sign"))
  }

}
