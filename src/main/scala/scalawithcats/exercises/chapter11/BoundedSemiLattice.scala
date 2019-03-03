package scalawithcats.exercises.chapter11

import cats.Monoid

// Idempotent Commutative Monoid also called a Bounded Semi Lattice
trait BoundedSemiLattice[A] extends Monoid[A] {

  def combine(a1: A, a2: A): A

  def empty: A

}

object BoundedSemiLattice {

  implicit val intInstance = new BoundedSemiLattice[Int] {
    override def combine(a1: Int, a2: Int): Int = a1 max a2
    override def empty: Int = 0
  }

  implicit def setInstance[B] = new BoundedSemiLattice[Set[B]] {
    override def combine(a1: Set[B], a2: Set[B]): Set[B] = a1 union a2
    override def empty: Set[B] = Set.empty[B]
  }

}
