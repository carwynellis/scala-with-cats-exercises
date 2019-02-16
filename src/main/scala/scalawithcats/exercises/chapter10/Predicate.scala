package scalawithcats.exercises.chapter10

import cats._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.implicits._

sealed trait Predicate[E, A] {

  def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

  def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {
    case Pure(func) => func(a)
    case And(left, right) => (left(a), right(a)).mapN((_, _) => a)
    case Or(left, right) => left(a) match {
      case Valid(a1)   => Valid(a)
      case Invalid(e1) =>
        right(a) match {
          case Valid(a2)   => Valid(a)
          case Invalid(e2) => Invalid(e2)
        }
    }
  }
}

final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
