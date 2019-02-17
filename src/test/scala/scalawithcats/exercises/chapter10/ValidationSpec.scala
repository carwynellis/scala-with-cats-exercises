package scalawithcats.exercises.chapter10

import org.scalatest.{FlatSpec, Matchers}
import Validation._
import cats.data.Validated.{Invalid, Valid}

class ValidationSpec extends FlatSpec with Matchers {

  behavior of "validateUsername"

  it should "accept a valid username string" in {
    validateUsername("ValidUsername") shouldBe Valid("ValidUsername")
  }

  it should "reject a username that is too short" in {
    validateUsername("Foo") shouldBe Invalid(error("Must be longer than 3 characters"))
  }

  it should "reject a username that does not contain only alphanumeric characters" in {
    validateUsername("FooBar!") shouldBe Invalid(error("Must be all alphanumeric characters"))
  }

}
