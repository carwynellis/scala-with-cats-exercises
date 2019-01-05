package scalawithcats.exercises.chapter04

import cats.data.Reader
import cats.implicits._

object Login {

  case class Db(
    usernames: Map[Int, String],
    passwords: Map[String, String]
  )

  private val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )
  private val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  type DbReader[A] = Reader[Db, A]

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    username <- findUsername(userId)
    valid <- username.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
  } yield valid

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(d => d.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(d => d.passwords.get(username).contains(password))

}
