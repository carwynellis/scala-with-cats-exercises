package scalawithcats.exercises.chapter10

import cats._
import cats.implicits._

trait Check[E, A] {

  def and(that: Check[E, A]): Check[E, A] = And(this, that)

  def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {
    case Pure(f) => f(a)
    case And(x, y) => (x(a), y(a)) match {
      case (Left(errorX), Left(errorY)) => Left(errorX |+| errorY)
      case (Left(errorX), Right(_))     => Left(errorX)
      case (Right(_), Left(errorY))     => Left(errorY)
      case (Right(_), Right(_))         => Right(a)
    }
  }

}

// Following ADT style in case study.
final case class Pure[E, A](f: A => Either[E, A]) extends Check[E, A]
final case class And[E, A](x: Check[E, A], y: Check[E, A]) extends Check[E, A]
