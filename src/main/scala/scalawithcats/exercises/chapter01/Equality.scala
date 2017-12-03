package scalawithcats.exercises.chapter01

import cats._
import cats.implicits._

object Equality extends App {

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    (cat1.name === cat2.name) &&
      (cat1.age === cat2.age) &&
      (cat1.color === cat1.color)
  }

  val cat1 = Cat("Garfield",   38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")

  println(s"$cat1 === $cat1: ${cat1 === cat1}")
  println(s"$cat1 === $cat2: ${cat1 === cat2}")
  println(s"$cat1 =!= $cat2: ${cat1 =!= cat2}")

  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(s"$optionCat1 === $optionCat2: ${optionCat1 === optionCat2}")

}
