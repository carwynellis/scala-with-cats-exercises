package scalawithcats.exercises.chapter10

import cats.implicits._
import cats.data.{Kleisli, NonEmptyList}

// Implementation of Validation using Kleisli instead of check
object ValidationKleisli {

  type Errors = NonEmptyList[String]
  type Result[A] = Either[Errors, A]
  type Check[A, B] = Kleisli[Result, A, B]

  // Create a check from a function:
  def check[A, B](func: A => Result[B]): Check[A, B] =
    Kleisli(func)
  // Create a check from a Predicate:
  def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli[Result, A, A](pred.run)

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

  val validateUsername: Check[String, String] = checkPred(longerThan(3)) andThen checkPred(alphanumeric)

  lazy val validateEmailAddress: Check[String, String] = ???

}
