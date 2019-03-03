package scalawithcats.exercises.chapter11

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._

class GCounterSpec extends FlatSpec with Matchers {

  behavior of "increment"

  it should "increment the count for the specified machine" in {
    GCounter(Map("A" -> 10)).increment("A", 1) shouldBe GCounter(Map("A" -> 11))
  }

  it should "add a new entry for a machine not present in counters" in {
    GCounter(Map.empty).increment("A", 1) shouldBe GCounter(Map("A" -> 1))
  }

  behavior of "merge"

  it should "combine entries from two counters" in {
    GCounter(Map("A" -> 1)).merge(GCounter(Map("B" -> 2))) shouldBe
      GCounter(Map("A" -> 1, "B" -> 2))
  }

  it should "accept the higher value for a given machine value" in {
    GCounter(Map("A" -> 1)).merge(GCounter(Map("A" -> 10))) shouldBe
      GCounter(Map("A" -> 10))
  }

  behavior of "total"

  it should "return zero for an empty counters map" in {
    GCounter[Int](Map.empty).total shouldBe 0
  }

  it should "return the sum of all counters" in {
    GCounter(Map("A" -> 1, "B" -> 2)).total shouldBe 3
  }

}
