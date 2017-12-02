package scalawithcats.exercises.chapter01

import cats._
import cats.implicits._

object PrintableWithShow extends App {

  final case class Cat(name: String, age: Int, color: String)

  implicit val showCat: Show[Cat] = Show.show { cat =>
    s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }

  val cat = Cat("Tigger", 12, "Brown")

  println(cat.show)

}
