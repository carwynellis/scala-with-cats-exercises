package scalawithcats.exercises.chapter09

import cats.Monoid

object FoldMap {

  def foldMap[A,B : Monoid](v: Vector[A])(f: A => B): B =
    v.map(f).fold(Monoid[B].empty)(Monoid[B].combine)

}
