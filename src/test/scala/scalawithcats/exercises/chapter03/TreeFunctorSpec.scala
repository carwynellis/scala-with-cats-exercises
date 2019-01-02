package scalawithcats.exercises.chapter03

import org.scalatest.{FlatSpec, Matchers}
import cats._
import TreeFunctor._

class TreeFunctorSpec extends FlatSpec with Matchers {

  behavior of "TreeFunctor"

  it should "apply function to leaf node" in {
    Functor[Tree].map(Leaf(1))(_ + 1) shouldBe Leaf(2)
  }

  it should "apply function to simple branch node" in {
    Functor[Tree].map(Branch(Leaf(1), Leaf(2)))(_ + 1) shouldBe Branch(Leaf(2), Leaf(3))
  }

  it should "apply function recursively over a more complex tree" in {
    Functor[Tree].map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_ + 1) shouldBe
      Branch(Branch(Leaf(2), Leaf(3)), Leaf(4))
  }

}
