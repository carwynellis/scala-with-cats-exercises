package scalawithcats.exercises.chapter06

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._

class ProductSpec extends FlatSpec with Matchers {

  behavior of "product"

  it should "return the catesian product of two lists" in {
    Product.product(List(1, 2), List(3, 4)) shouldBe List((1, 3), (1, 4), (2, 3), (2, 4))
  }

}
