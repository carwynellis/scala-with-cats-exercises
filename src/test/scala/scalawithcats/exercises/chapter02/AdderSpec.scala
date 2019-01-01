package scalawithcats.exercises.chapter02

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._

class AdderSpec extends FlatSpec with Matchers {

  behavior of "add"

  it should "return the sum of a list of ints" in {
    Adder.add(List(1, 2, 3)) shouldBe 6
  }

  it should "return the sum of a list of option ints" in {
    Adder.add(List(Option(1), Option(2), Option(3))) shouldBe Some(6)
  }

  it should "return the sum of a list of orders" in {
    import Adder._

    Adder.add(List(Order(1, 1), Order(1, 1))) shouldBe Order(2, 2)
  }

}
