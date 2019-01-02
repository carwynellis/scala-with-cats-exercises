package scalawithcats.exercises.chapter03

import org.scalatest.{FlatSpec, Matchers}
import PrintableInstances._
import Printable._

class PrintableSpec extends FlatSpec with Matchers {

  behavior of "format"

  it should "return a formatted string" in {
    format("Foo") shouldBe "\"Foo\""
  }

  it should "return a formatted boolean" in {
    format(true) shouldBe "yes"
  }

  it should "return a formatted box" in {
    format(Box("Foo")) shouldBe "\"Foo\""
  }

}
