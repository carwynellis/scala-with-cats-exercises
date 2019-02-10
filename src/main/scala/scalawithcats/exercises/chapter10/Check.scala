package scalawithcats.exercises.chapter10

import cats._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._

trait Check[E, A] {

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def or(that: Check[E, A]): Check[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(f) => f(a)
    case And(x, y) => (x(a), y(a)).mapN((_, _) => a)
    case Or(x, y) => x(a) match {
      case Valid(_) => Valid(a)
      case Invalid(_) => y(a) match {
        case Valid(_) => Valid(a)
        case Invalid(yError) => Invalid(yError)
      }
    }
  }

}

// Following ADT style in case study.
final case class Pure[E, A](f: A => Validated[E, A]) extends Check[E, A]
final case class And[E, A](x: Check[E, A], y: Check[E, A]) extends Check[E, A]
final case class Or[E, A](x: Check[E, A], y: Check[E, A]) extends Check[E, A]
