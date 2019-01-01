package scalawithcats.exercises.chapter02

import org.scalatest.{FlatSpec, Matchers}

class BooleanSpec extends FlatSpec with Matchers {

  behavior of "booleanAnd"

  it should "return true when combining two true values" in {
    Monoid(Boolean.booleanAnd).combine(true, true) shouldBe true
  }

  it should "return false when combining a true and false value" in {
    Monoid(Boolean.booleanAnd).combine(true, false) shouldBe false
  }

  it should "return false when combining a false and true value" in {
    Monoid(Boolean.booleanAnd).combine(false, true) shouldBe false
  }

  it should "return false when combining two false values" in {
    Monoid(Boolean.booleanAnd).combine(false, false) shouldBe false
  }

  it should "obey the identity law" in {
    import Boolean.booleanAnd

    Seq(true, false) map { b => BooleanSpec.identityLaw(b) shouldBe true }
  }

  it should "obey the associative law" in {
    import Boolean.booleanAnd

    BooleanSpec.BooleanPermutations map {
      case (x, y, z) => BooleanSpec.associativeLaw(x, y, z) shouldBe true
    }
  }

  behavior of "booleanOr"

  it should "return true when combining two true values" in {
    Monoid(Boolean.booleanOr).combine(true, true) shouldBe true
  }

  it should "return true when combining a true and false value" in {
    Monoid(Boolean.booleanOr).combine(true, false) shouldBe true
  }

  it should "return true when combining a false and true value" in {
    Monoid(Boolean.booleanOr).combine(false, true) shouldBe true
  }

  it should "return false when combining two false values" in {
    Monoid(Boolean.booleanOr).combine(false, false) shouldBe false
  }

  it should "obey the identity law" in {
    import Boolean.booleanOr

    Seq(true, false) map { b => BooleanSpec.identityLaw(b) shouldBe true }
  }

  it should "obey the associative law" in {
    import Boolean.booleanOr

    BooleanSpec.BooleanPermutations map {
      case (x, y, z) => BooleanSpec.associativeLaw(x, y, z) shouldBe true
    }
  }

}

object BooleanSpec {

  val BooleanPermutations = for {
    a <- Seq(true, false)
    b <- Seq(true, false)
    c <- Seq(true, false)
  } yield (a, b, c)

  def associativeLaw[A](x: A, y: A, z: A)(implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)(implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }

}
