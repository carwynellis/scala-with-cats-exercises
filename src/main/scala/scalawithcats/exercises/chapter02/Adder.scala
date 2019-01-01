package scalawithcats.exercises.chapter02

import cats.implicits._

object Adder {

  def add[A](items: List[A])(implicit m: cats.Monoid[A]) = items.fold(m.empty)(_ |+| _)

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid = new cats.Monoid[Order] {
    override def empty: Order = Order(0, 0)
    override def combine(x: Order, y: Order): Order = Order(
      totalCost = x.totalCost + y.totalCost,
      quantity = x.quantity + y.quantity
    )
  }

}
