package scalawithcats.exercises.chapter03

import org.scalatest.{FlatSpec, Matchers}

import CodecInstances._
import Codec._

class CodecSpec extends FlatSpec with Matchers {

  behavior of "encode"

  it should "encode an Int to String" in {
    encode(123) shouldBe "123"
  }

  it should "encode a Boolean to String" in {
    encode(true) shouldBe "true"
  }

  it should "encode a Double to String" in {
    encode(123.4) shouldBe "123.4"
  }

  it should "encode a Box(Double) to String" in {
    encode(Box(123.4)) shouldBe "123.4"
  }

  behavior of "decode"

  it should "decode a String to Int" in {
    decode[Int]("123") shouldBe 123
  }

  it should "decode a String to Boolean" in {
    decode[Boolean]("true") shouldBe true
  }

  it should "decode a String to Double" in {
    decode[Double]("123.4") shouldBe 123.4
  }

  it should "decode a String to Box[Double]" in {
    decode[Box[Double]]("123.4") shouldBe Box(123.4)
  }
}
