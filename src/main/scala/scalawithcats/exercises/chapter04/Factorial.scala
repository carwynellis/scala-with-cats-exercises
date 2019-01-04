package scalawithcats.exercises.chapter04

import cats.implicits._
import cats.data.Writer

object Factorial {

  def slowly[A](body: => A) = try body finally Thread.sleep(100)

  def factorial(n: Int): Writer[List[String], Int] = {

    val result: Writer[List[String], Int] =
      slowly {
        if (n == 0) Writer.value[List[String], Int](1)
        else for {
          r <- factorial(n - 1)
        } yield n * r
      }

    for {
      a <- result
      r <- Writer(List(s"fact $n $a"), a)
    } yield r

  }

}
