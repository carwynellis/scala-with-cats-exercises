package scalawithcats.exercises.chapter08

import cats.implicits._
import cats.Applicative

import scala.language.higherKinds

class UptimeService[F[_] : Applicative](client: UptimeClient[F]) {

  def getTotalUptime(hostnames: List[String]): F[Int] = hostnames.traverse(client.getUptime).map(_.sum)

}
