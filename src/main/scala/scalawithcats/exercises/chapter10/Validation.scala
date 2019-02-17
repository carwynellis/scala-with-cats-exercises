package scalawithcats.exercises.chapter10

import cats.data.NonEmptyList
import scalawithcats.exercises.chapter10.Check.{AndThen, Pure}

object Validation {

  type Errors = NonEmptyList[String]

  def error(s: String): NonEmptyList[String] = NonEmptyList(s, Nil)

  private def longerThan(n: Int): Predicate[Errors, String] = Predicate.lift(
      error(s"Must be longer than $n characters"),
      str => str.length > n
  )

  private val alphanumeric: Predicate[Errors, String] = Predicate.lift(
    error(s"Must be all alphanumeric characters"),
    str => str.forall(_.isLetterOrDigit)
  )

  private def contains(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character $char"),
    str => str.contains(char)
  )

  private def containsOnce(char: Char): Predicate[Errors, String] = Predicate.lift(
    error(s"Must contain the character $char only once"),
    str => str.count(c => c == char) == 1
  )

  val validateUsername = AndThen(Pure(longerThan(3)), Pure(alphanumeric))

}
