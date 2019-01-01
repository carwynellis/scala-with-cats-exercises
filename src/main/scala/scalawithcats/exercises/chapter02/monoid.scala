package scalawithcats.exercises.chapter02

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) = monoid
}

object Semigroup {
  def apply[A](implicit semigroup: Semigroup[A]) = semigroup
}
