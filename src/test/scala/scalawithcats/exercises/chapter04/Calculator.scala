package scalawithcats.exercises.chapter04

import cats.data.State
import cats.implicits._

object Calculator {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = sym match {
    case "+" => calculate(_ + _)
    case "*" => calculate(_ * _)
    case "-" => calculate(_ - _)
    case "/" => calculate(_ / _)
    case i   => addOperand(i.toInt)
  }

  def evalAll(input: List[String]): CalcState[Int] =
    input.foldLeft(0.pure[CalcState]) { (state, symbol) =>
      state.flatMap(_ => evalOne(symbol))
    }

  private def calculate(f: (Int, Int) => Int): CalcState[Int] = State {
    case a :: b :: tail =>
      val result = f(a, b)
      (result :: tail, result)
    case s => throw new IllegalStateException(s"Not enough operands on stack: $s")
  }

  private def addOperand(op: Int): CalcState[Int] = State { stack => (op :: stack, op) }

}
