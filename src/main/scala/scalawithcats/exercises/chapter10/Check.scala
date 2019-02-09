package scalawithcats.exercises.chapter10

import cats._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._

trait Check[E, A] {

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(f) => f(a)
    case And(x, y) => (x(a), y(a)) match {
      case (Invalid(errorX), Invalid(errorY)) => Invalid(errorX |+| errorY)
      case (Invalid(errorX), Valid(_))        => Invalid(errorX)
      case (Valid(_), Invalid(errorY))        => Invalid(errorY)
      case (Valid(_), Valid(_))               => Valid(a)
    }
  }

}

// Following ADT style in case study.
final case class Pure[E, A](f: A => Validated[E, A]) extends Check[E, A]
final case class And[E, A](x: Check[E, A], y: Check[E, A]) extends Check[E, A]
