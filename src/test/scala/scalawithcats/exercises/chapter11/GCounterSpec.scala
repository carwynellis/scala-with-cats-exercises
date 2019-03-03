package scalawithcats.exercises.chapter11

import org.scalatest.{FlatSpec, Matchers}
import cats.implicits._

class GCounterSpec extends FlatSpec with Matchers {

  val underTest = GCounter[Map, String, Int]

  behavior of "increment"

  it should "increment the count for the specified machine" in {
    underTest.increment(Map("A" -> 10))("A", 1) shouldBe Map("A" -> 11)
  }

  it should "add a new entry for a machine not present in counters" in {
    underTest.increment(Map.empty)("A", 1) shouldBe Map("A" -> 1)
  }

  behavior of "merge"

  it should "combine entries from two counters" in {
    underTest.merge(Map("A" -> 1), Map("B" -> 2)) shouldBe Map("A" -> 1, "B" -> 2)
  }

  it should "accept the higher value for a given machine value" in {
    underTest.merge(Map("A" -> 1), Map("A" -> 10)) shouldBe Map("A" -> 10)
  }

  behavior of "total"

  it should "return zero for an empty counters map" in {
    underTest.total(Map.empty) shouldBe 0
  }

  it should "return the sum of all counters" in {
    underTest.total(Map("A" -> 1, "B" -> 2)) shouldBe 3
  }

}
