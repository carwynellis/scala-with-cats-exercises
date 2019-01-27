package scalawithcats.exercises.chapter08

import org.scalatest.{FlatSpec, Matchers}

class UptimeServiceSpec extends FlatSpec with Matchers {

  private val hostUptimes = Map(
    "foo.bar.com"     -> 12,
    "www.example.com" -> 30
  )

  private val testClient = new TestUptimeClient(hostUptimes)

  private val underTest = new UptimeService(testClient)

  behavior of "getTotalUptime"

  it should "return the total uptime for all hosts" in {
    underTest.getTotalUptime(hostUptimes.keys.toList) shouldBe 42
  }

}
