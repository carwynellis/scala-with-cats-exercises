package scalawithcats.exercises.chapter06

import org.scalatest.{FlatSpec, Matchers}
import FormValidation._
import cats.data.Validated.{Invalid, Valid}

class FormValidationSpec extends FlatSpec with Matchers {

  behavior of "readName"

  it should "return a name string for a form containing a valid name string" in {
    readName(Map(Fields.Name -> "Foo")) shouldBe Right("Foo")
  }

  it should "return an error if no name was present in the form" in {
    readName(Map.empty) shouldBe Left(List(s"Field: ${Fields.Name} not present in form data"))
  }

  it should "return an error if the name is present in the form but is blank" in {
    readName(Map(Fields.Name -> "")) shouldBe Left(List("String field must contain a value"))
  }

  behavior of "readAge"

  it should "return age as an Integer where a valid value is present in the form" in {
    readAge(Map(Fields.Age -> "42")) shouldBe Right(42)
  }

  it should "return an error if no age was present in the form" in {
    readAge(Map()) shouldBe Left(List(s"Field: ${Fields.Age} not present in form data"))
  }

  it should "return an error if an invalid age value is present in the form" in {
    readAge(Map(Fields.Age -> "-100")) shouldBe Left(List("Integer value must not be negative"))
  }

  it should "return an error if the age value could not be parsed as an Integer value" in {
    readAge(Map(Fields.Age -> "Foo")) shouldBe Left(List("Failed to parse string 'Foo' into an Integer value"))
  }

  behavior of "createUser"

  it should "return a user from a form containing a valid name and age" in {
    createUser(Map(Fields.Name -> "Foo", Fields.Age -> "42")) shouldBe Valid(User("Foo", 42))
  }

  it should "return errors indicating that name and age are not present for an empty form" in {
    createUser(Map.empty) shouldBe Invalid(List(Fields.Name, Fields.Age)
      .map(f => s"Field: $f not present in form data"
    ))
  }

}
