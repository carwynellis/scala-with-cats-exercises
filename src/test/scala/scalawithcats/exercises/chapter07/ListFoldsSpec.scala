package scalawithcats.exercises.chapter07

import org.scalatest.{FlatSpec, Matchers}

class ListFoldsSpec extends FlatSpec with Matchers {

  behavior of "foldListLeft"

  it should "return a reversed list" in {
    ListFolds.foldListLeft(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  behavior of "foldListRight"

  it should "return the list with elements in the same order" in {
    ListFolds.foldListRight(List(1, 2, 3)) shouldBe List(1, 2, 3)
  }

}
