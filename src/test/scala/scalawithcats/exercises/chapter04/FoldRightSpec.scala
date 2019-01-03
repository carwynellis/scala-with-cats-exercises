package scalawithcats.exercises.chapter04

import org.scalatest.{FlatSpec, Matchers}

class FoldRightSpec extends FlatSpec with Matchers {

  behavior of "foldRight"

  it should "fold over a small list" in {
    FoldRight.foldRight(List(1, 2, 3), 0)((i, acc) => i + acc).value shouldBe 6
  }

  it should "fold successfully over a larger list without triggering a stack overflow" in {
    FoldRight.foldRight(List.fill(1000000)(1), 0)((i, acc) => i + acc).value shouldBe 1000000
  }

}
