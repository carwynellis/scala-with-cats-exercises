package scalawithcats.exercises.chapter02

object SetInstances {

  // Union
  implicit def setUnion[A] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  // Intersection - does not form a monoid since there is no identity value.
  implicit def setIntersection[A] = new Semigroup[Set[A]] {
    override def combine(x: Set[A], y: Set[A]): Set[A] = x intersect y
  }

  // Note - Set difference is not associative so does not form a monad

  // The solutions also provide a monoid for Symmetric difference (union less intersection)
}
