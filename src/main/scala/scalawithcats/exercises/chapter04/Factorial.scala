package scalawithcats.exercises.chapter04

import cats.implicits._
import cats.data.Writer

object Factorial {

  private def slowly[A](body: => A) = try body finally Thread.sleep(100)

  type Logged[A] = Writer[List[String], A]

  def factorial(n: Int): Logged[Int] = {

    val result = slowly {
      if (n == 0) 1.pure[Logged]
      else for {
        r <- factorial(n - 1)
      } yield n * r
    }

    for {
      a <- result
      _ <- List(s"fact $n $a").tell
    } yield a

  }

}
