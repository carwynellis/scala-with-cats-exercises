package scalawithcats.exercises.chapter02

import org.scalatest.{FlatSpec, Matchers}

class BooleanInstancesSpec extends FlatSpec with Matchers {

  private val BooleanPermutations = for {
    a <- Seq(true, false)
    b <- Seq(true, false)
    c <- Seq(true, false)
  } yield (a, b, c)

  behavior of "booleanAnd"

  it should "return true when combining two true values" in {
    Monoid(BooleanInstances.booleanAnd).combine(true, true) shouldBe true
  }

  it should "return false when combining a true and false value" in {
    Monoid(BooleanInstances.booleanAnd).combine(true, false) shouldBe false
  }

  it should "return false when combining a false and true value" in {
    Monoid(BooleanInstances.booleanAnd).combine(false, true) shouldBe false
  }

  it should "return false when combining two false values" in {
    Monoid(BooleanInstances.booleanAnd).combine(false, false) shouldBe false
  }

  it should "obey the identity law" in {
    import BooleanInstances.booleanAnd

    Seq(true, false) map { b => MonoidLaws.identityLaw(b) shouldBe true }
  }

  it should "obey the associative law" in {
    import BooleanInstances.booleanAnd

    BooleanPermutations map {
      case (x, y, z) => MonoidLaws.associativeLaw(x, y, z) shouldBe true
    }
  }

  behavior of "booleanOr"

  it should "return true when combining two true values" in {
    Monoid(BooleanInstances.booleanOr).combine(true, true) shouldBe true
  }

  it should "return true when combining a true and false value" in {
    Monoid(BooleanInstances.booleanOr).combine(true, false) shouldBe true
  }

  it should "return true when combining a false and true value" in {
    Monoid(BooleanInstances.booleanOr).combine(false, true) shouldBe true
  }

  it should "return false when combining two false values" in {
    Monoid(BooleanInstances.booleanOr).combine(false, false) shouldBe false
  }

  it should "obey the identity law" in {
    import BooleanInstances.booleanOr

    Seq(true, false) map { b => MonoidLaws.identityLaw(b) shouldBe true }
  }

  it should "obey the associative law" in {
    import BooleanInstances.booleanOr

    BooleanPermutations map {
      case (x, y, z) => MonoidLaws.associativeLaw(x, y, z) shouldBe true
    }
  }

}

object BooleanInstancesSpec {


}
