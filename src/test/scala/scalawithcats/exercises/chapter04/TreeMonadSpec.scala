package scalawithcats.exercises.chapter04

import org.scalatest.{FlatSpec, Matchers}
import TreeMonad._

class TreeMonadSpec extends FlatSpec with Matchers {

  behavior of "pure"

  it should "return a leaf containing the specified value" in {
    treeMonad.pure(1) shouldBe Leaf(1)
  }

  behavior of "flatMap"

  it should "apply function to all nodes of the tree" in {
    treeMonad.flatMap(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))(v => Leaf(v + 1)) shouldBe
      Branch(Leaf(2), Branch(Leaf(3), Leaf(4)))
  }

}
