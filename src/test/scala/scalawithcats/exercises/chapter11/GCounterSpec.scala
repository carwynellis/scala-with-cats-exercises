package scalawithcats.exercises.chapter11

import org.scalatest.{FlatSpec, Matchers}
import GCounter._
import cats.instances.map._

// TODO
//   * we cannot have the BoundedSemiLattice int instance in scope with the catsKernetStdGroupForInst instance since
//     both share the type Monoid[Int]
//   * scoping the implicits separately provides the desired behaviour
//   * Is this the only way we can do this? We *could* provide a separate Monoid for incrementing so we could have this
//     in scope with the BoundedSemiLattice instance but this is more boilerplate duplicating the instances that Cats
//     provides.
class GCounterSpec extends FlatSpec with Matchers {

  val underTest = {
    import BoundedSemiLattice._
    GCounter[Map, String, Int]
  }

  behavior of "increment"

  {
    import cats.instances.int.catsKernelStdGroupForInt

    it should "increment the count for the specified machine" in {
      underTest.increment(Map("A" -> 10))("A", 1) shouldBe Map("A" -> 11)
    }

    it should "add a new entry for a machine not present in counters" in {
      underTest.increment(Map.empty)("A", 1) shouldBe Map("A" -> 1)
    }
  }

  behavior of "merge"

  {
    import BoundedSemiLattice._

    it should "combine entries from two counters" in {
      underTest.merge(Map("A" -> 1), Map("B" -> 2)) shouldBe Map("A" -> 1, "B" -> 2)
    }

    it should "accept the higher value for a given machine value" in {
      underTest.merge(Map("A" -> 1), Map("A" -> 10)) shouldBe Map("A" -> 10)
    }
  }

  behavior of "total"

  {
    import cats.instances.int.catsKernelStdGroupForInt

    it should "return zero for an empty counters map" in {
      underTest.total(Map.empty) shouldBe 0
    }

    it should "return the sum of all counters" in {
      underTest.total(Map("A" -> 1, "B" -> 2)) shouldBe 3
    }
  }

}
