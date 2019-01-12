package scalawithcats.exercises.chapter05

import cats.implicits._
import cats.data.EitherT

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Transformers {

  private val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autoBot: String): Response[Int] = {
    val result = Future {
      powerLevels.get(autoBot)
        .fold[Either[String, Int]](Left(s"Autobot: $autoBot unreachable"))(Right(_))
    }
    EitherT(result)
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = for {
    level1 <- getPowerLevel(ally1)
    level2 <- getPowerLevel(ally2)
  } yield (level1 + level2) > 15

  def tacticalReport(ally1: String, ally2: String): Future[String] = canSpecialMove(ally1, ally2).value.map {
    case Right(true)  => s"$ally1 and $ally2 are able to perform a special move"
    case Right(false) => s"$ally1 and $ally2 are unable to perform a special move"
    case Left(error)  => s"Caught error: $error"
  }

}
