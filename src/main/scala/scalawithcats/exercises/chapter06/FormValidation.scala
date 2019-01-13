package scalawithcats.exercises.chapter06

import cats.syntax.apply._
import cats.syntax.either._
import cats.data.Validated
import cats.instances.list._

object FormValidation {

  case class User(name: String, age: Int)

  object Fields {
    val Name = "name"
    val Age = "age"
  }

  def readName(form: Map[String, String]): Either[List[String], String] = for {
    value <- getValue(Fields.Name, form)
    name  <- nonBlank(value)
  } yield name

  def readAge(form: Map[String, String]): Either[List[String], Int] = for {
    value  <- getValue(Fields.Age, form)
    parsed <- parseInt(value)
    age    <- nonNegative(parsed)
  } yield age

  def createUser(form: Map[String, String]): Validated[List[String], User] = (
    readName(form).toValidated,
    readAge(form).toValidated
  ).mapN(User)

  private def getValue(field: String, form: Map[String, String]): Either[List[String], String] =
    form.get(field).toRight(List(s"Field: $field not present in form data"))

  private def parseInt(i: String): Either[List[String], Int] =
    Either.catchOnly[NumberFormatException](i.toInt).leftMap(_ => List(s"Failed to parse string '$i' into an Integer value"))

  private def nonBlank(s: String): Either[List[String], String] =
    Right(s).ensure(List("String field must contain a value"))(!_.isEmpty)

  private def nonNegative(i: Int): Either[List[String], Int] =
    Right(i).ensure(List("Integer value must not be negative"))(_ >= 0)
}
