package scalawithcats.exercises.chapter10

import cats.data.NonEmptyList
import scalawithcats.exercises.chapter10.Check.Pure

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

  val validateUsername = Pure(longerThan(3)) andThen Pure(alphanumeric)

  // TODO - express in terms of existing predicates?
  val validateEmailAddress = Pure(contains('@'))
    .map(_.split('@'))
    .andThen(Pure(Predicate.lift(error("Must contain characters before the @ sign"), a => a.headOption.forall(_.length > 0))))
    .andThen(Pure(Predicate.lift(error("Must contain a . after the @ sign"), b => b.tail.headOption.forall(_.contains('.')))))
    .andThen(Pure(Predicate.lift(error("Must contain at least 3 characters after the @ sign"), b => b.tail.headOption.forall(_.length > 3))))
    .map(_.mkString("@"))
}
