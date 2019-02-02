package scalawithcats.exercises.chapter09

import cats.implicits._
import org.scalatest.{AsyncFlatSpec, Matchers}

class FoldMapSpec extends AsyncFlatSpec with Matchers {

  behavior of "foldMap"

  it should "apply a function to the input sequence and fold over the result" in {
    FoldMap.foldMap(Vector(1, 2, 3))(_ + 1) shouldBe 9
  }

  behavior of "parallelFoldMap"

  it should "apply a function to the input sequence and fold over the result" in {
    FoldMap.parallelFoldMap(Vector(1, 2, 3))(_ + 1) map { result => result shouldBe 9 }
  }

}
