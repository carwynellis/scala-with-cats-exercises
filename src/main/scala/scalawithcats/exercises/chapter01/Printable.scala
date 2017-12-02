package scalawithcats.chapter01

trait Printable[A] {

  def format(value: A): String

}

object PrintableInstances {

  implicit val printableString = new Printable[String] {
    override def format(value: String) = value
  }

  implicit val printableInt = new Printable[Int] {
    override def format(value: Int) = value.toString
  }

  implicit val printableCat = new Printable[Cat] {
    override def format(cat: Cat) = s"${cat.name} is a ${cat.age} year-old ${cat.color} cat."
  }

}

object Printable {

  def format[A](value: A)(implicit printable: Printable[A]): String = printable.format(value)

  def print[A](value: A)(implicit printable: Printable[A]): Unit = println(format(value))

}

object PrintableSyntax {

  implicit class PrintableOps[A](value: A) {

    def format(implicit printable: Printable[A]) = printable.format(value)

    def print(implicit printable: Printable[A]) = println(format)

  }

}

final case class Cat(name: String, age: Int, color: String)

object PrintableCat extends App {

  import PrintableInstances._

  val cat = Cat("Tigger", 12, "Brown")

  Printable.print(cat)

}

object PrintableCatWithSyntax extends App {

  import PrintableInstances._
  import PrintableSyntax._

  val cat = Cat("Tigger", 12, "Brown")

  cat.print
}