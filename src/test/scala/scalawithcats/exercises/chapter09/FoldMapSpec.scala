package scalawithcats.exercises.chapter09

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._

class FoldMapSpec extends FlatSpec with Matchers {

  behavior of "foldMap"

  it should "apply a function to the input sequence and fold over the reuslt" in {
    FoldMap.foldMap(Vector(1, 2, 3))(_ + 1) shouldBe 9
  }

}
