package scalawithcats.exercises.chapter10

import cats._
import cats.data.Validated
import Check._

sealed trait Check[E, A, B] {

  def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

  def map[C](f: B => C): Check[E, A, C] = Map(this, f)

}

object Check {

  def apply[E, A](p: Predicate[E, A]): Check[E, A, A] = Pure(p)

  case class Pure[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
    def apply(a: A)(implicit s: Semigroup[E]) = p(a)
  }

  case class Map[E, A, B, C](c: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]) = c(a).map(f)
  }
}

