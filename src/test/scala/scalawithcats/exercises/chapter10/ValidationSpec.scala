package scalawithcats.exercises.chapter10

import org.scalatest.{FlatSpec, Matchers}
import Validation._
import cats.data.Validated.{Invalid, Valid}

class ValidationSpec extends FlatSpec with Matchers {

  behavior of "validateUsername"

  it should "accept a valid username" in {
    validateUsername("ValidUsername") shouldBe Valid("ValidUsername")
  }

  it should "reject a username that is too short" in {
    validateUsername("Foo") shouldBe Invalid(error("Must be longer than 3 characters"))
  }

  it should "reject a username that does not contain only alphanumeric characters" in {
    validateUsername("FooBar!") shouldBe Invalid(error("Must be all alphanumeric characters"))
  }

  behavior of "validateEmailAddress"

  it should "accept a valid email address" in {
    validateEmailAddress("foo@example.com") shouldBe Valid("foo@example.com")
  }

  it should "reject an email address that does not contain an @ symbol" in {
    validateEmailAddress("example.com") shouldBe Invalid(error("Must contain the character @"))
  }

  it should "reject an email address that contains no characters before the @ sign" in {
    validateEmailAddress("@example.com") shouldBe Invalid(error("Must contain characters before the @ sign"))
  }

  it should "reject an email address that contains less than three characters after the @ sign" in {
    validateEmailAddress("foo@e.") shouldBe Invalid(error("Must contain at least 3 characters after the @ sign"))
  }

  it should "reject an email address that does not contain a . in the string following the @ sign" in {
    validateEmailAddress("foo@examplecom") shouldBe Invalid(error("Must contain a . after the @ sign"))
  }

}
