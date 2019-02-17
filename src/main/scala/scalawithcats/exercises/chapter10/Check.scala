package scalawithcats.exercises.chapter10

import cats.data.Validated
import Check._
import cats.Semigroup

sealed trait Check[E, A, B] {

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map(this, f)

  def flatMap[C](f: B => Check[E, A, C]) = FlatMap(this, f)

  def andThen[C](that: Check[E, B, C]): Check[E, A, C] = AndThen(this, that)

}

object Check {

  def apply[E, A](p: Predicate[E, A]): Check[E, A, A] = Pure(p)

  case class Pure[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]) = p(a)
  }

  case class Map[E, A, B, C](c: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]) = c(a).map(f)
  }

  case class FlatMap[E, A, B, C](c: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]) = c(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  case class AndThen[E, A, B, C](c1: Check[E, A, B], c2: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]) = c1(a).withEither(_.flatMap(c2(_).toEither))
  }

}

