package scalawithcats.exercises.chapter10

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

  it should "accept a valid email address" in {
    validateEmailAddress("foo@example.com") shouldBe Right("foo@example.com")
  }

  it should "reject an email address that does not contain an @ symbol" in {
    validateEmailAddress("example.com") shouldBe Left(error("Must contain the character @ only once"))
  }

  it should "reject an email address that contains no characters before the @ sign" in {
    validateEmailAddress("@example.com") shouldBe Left(error("Must contain characters before the @ sign"))
  }

  it should "reject an email address that contains less than three characters after the @ sign" in {
    validateEmailAddress("foo@e.") shouldBe Left(error("Must contain at least 3 characters after the @ sign"))
  }

  it should "reject an email address that does not contain a . in the string following the @ sign" in {
    validateEmailAddress("foo@examplecom") shouldBe Left(error("Must contain a . after the @ sign"))
  }

}
