package scalawithcats.exercises.chapter11

import cats.Monoid
import cats.implicits._

final case class GCounter[A](counters: Map[String, A]) {

  def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] =
    GCounter(counters.updated(machine, amount |+| counters.getOrElse(machine, m.empty)))

  def merge(that: GCounter[A])(implicit m: BoundedSemiLattice[A]): GCounter[A] = GCounter[A](counters |+| that.counters)

  def total(implicit m: Monoid[A]): A = counters.values.fold(m.empty)(_ |+| _)

}
