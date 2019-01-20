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

  behavior of "map"

  it should "apply function to each element of the list preserving original list order" in {
    ListFolds.map(List(1, 2, 3))(_ + 1) shouldBe List(2, 3, 4)
  }

  behavior of "flatMap"

  it should "apply function to each element of the list preserving original list order" in {
    ListFolds.flatMap(List(1, 2, 3))(i => List(i + 1)) shouldBe List(2, 3, 4)
  }

  behavior of "filter"

  it should "retain elements of the list that satisfy the supplied predicate" in {
    ListFolds.filter(List(1, 2, 3))(_ % 2 == 1) shouldBe List(1, 3)
  }

  behavior of "sum"

  it should "return the sum of the list" in {
    import cats.instances.int._
    ListFolds.sum(List(1, 2, 3)) shouldBe 6
  }

}
