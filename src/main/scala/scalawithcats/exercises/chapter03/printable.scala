package scalawithcats.exercises.chapter03

trait Printable[A] { self =>
  def format(value: A): String
  def contramap[B](func: B => A): Printable[B] = new Printable[B] {
    // We invoke format on the out printable to convert the result of func() into a String
    def format(value: B): String = self.format(func(value))
  }
}

object PrintableInstances {

  implicit val stringPrintable: Printable[String] =
    new Printable[String] {
      def format(value: String): String =
        "\"" + value + "\""
    }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
    def format(value: Boolean): String =
      if(value) "yes" else "no"
  }

  implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
    p.contramap[Box[A]](_.value)
}

object Printable {
  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)
}




