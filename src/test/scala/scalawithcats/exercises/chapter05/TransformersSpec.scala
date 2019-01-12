package scalawithcats.exercises.chapter05

import org.scalatest.{AsyncFlatSpec, Matchers}

class TransformersSpec extends AsyncFlatSpec with Matchers {

  behavior of "getPowerLevel"

  it should "return a result for a valid autoBot name" in {
    val result = Transformers.getPowerLevel("Bumblebee")
    result.value.map { r => r shouldBe Right(8) }
  }

  it should "return an error message for an invalid bot name" in {
    val result = Transformers.getPowerLevel("invalid name")
    result.value.map { r => r shouldBe Left("Autobot: invalid name unreachable")}
  }

  behavior of "canSpecialMove"

  it should "return true for two allies with combined power level > 15" in {
    val result = Transformers.canSpecialMove("Bumblebee", "Hot Rod")
    result.value.map { r => r shouldBe Right(true) }
  }

  it should "return false for two allies with combined power level <= 15" in {
    val result = Transformers.canSpecialMove("Bumblebee", "Jazz")
    result.value.map { r => r shouldBe Right(false) }
  }

  behavior of "tacticalreport"

  it should "return a suitable message if both bots can perform a special move" in {
    val result = Transformers.tacticalReport("Bumblebee", "Hot Rod")
    result.map { r => r shouldBe "Bumblebee and Hot Rod are able to perform a special move" }
  }

  it should "return a suitable message if both bots are unable to perform a special move" in {
    val result = Transformers.tacticalReport("Bumblebee", "Jazz")
    result.map { r => r shouldBe "Bumblebee and Jazz are unable to perform a special move" }
  }

  it should "return an error message if an error occurred" in {
    val result = Transformers.tacticalReport("Bumblebee", "invalid")
    result.map { r => r shouldBe "Caught error: Autobot: invalid unreachable" }
  }

}
