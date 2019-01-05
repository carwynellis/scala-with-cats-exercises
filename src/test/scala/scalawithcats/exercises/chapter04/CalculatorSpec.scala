package scalawithcats.exercises.chapter04

import org.scalatest.{FlatSpec, Matchers}

class CalculatorSpec extends FlatSpec with Matchers {

  behavior of "evalOne"

  it should "return the operand where the stack is currently empty" in {
    Calculator.evalOne("42").run(Nil).value shouldBe (List(42), 42)
  }

  it should "apply addition for a valid stack where the current symbol is +" in {
    Calculator.evalOne("+").run(List(1, 2)).value shouldBe (List(3), 3)
  }

  it should "apply multiplication for a valid stack where the current symbol is *" in {
    Calculator.evalOne("*").run(List(1, 2)).value shouldBe (List(2), 2)
  }

  it should "apply subtraction for a valid stack where the current symbol is -" in {
    Calculator.evalOne("-").run(List(2, 1)).value shouldBe (List(1), 1)
  }

  it should "apply division for a valid stack where the current symbol is /" in {
    Calculator.evalOne("/").run(List(8, 4)).value shouldBe (List(2), 2)
  }

  behavior of "evalAll"

  it should "compute the correct result for a valid sequence of operands" in {
    Calculator.evalAll(List("1", "2", "+", "3", "*")).runA(Nil).value shouldBe 9
  }

}
