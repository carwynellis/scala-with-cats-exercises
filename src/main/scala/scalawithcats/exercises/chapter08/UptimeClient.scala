package scalawithcats.exercises.chapter08

import cats.Id

import scala.concurrent.Future
import scala.language.higherKinds

trait UptimeClient[F[_]] {

  def getUptime(hostname: String): F[Int]

}

trait RealUptimeClient extends UptimeClient[Future] {

  override def getUptime(hostname: String): Future[Int] = ???

}

class TestUptimeClient(hostUptimes: Map[String, Int]) extends UptimeClient[Id] {

  override def getUptime(hostname: String): Id[Int] = hostUptimes.getOrElse(hostname, 0)

}
