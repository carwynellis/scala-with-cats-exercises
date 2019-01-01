package scalawithcats.exercises.chapter02

object Boolean {

  implicit val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = true
    def combine(x: Boolean, y: Boolean) = x && y
  }

  implicit val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def empty = false
    def combine(x: Boolean, y: Boolean) = x || y
  }

  // The solutions also provide monoids for exclusiveOr and exclusiveNor
}
