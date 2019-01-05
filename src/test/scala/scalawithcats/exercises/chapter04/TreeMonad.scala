package scalawithcats.exercises.chapter04

object TreeMonad {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  val treeMonad = new Monad[Tree] {

    override def pure[A](a: A): Tree[A] = Leaf(a)

    // This is not tail recursive. Cats provides support for tail recursive flatMap through
    // tailRecM on FlatMap. See https://typelevel.org/cats/faq.html#tailrecm
    override def flatMap[A, B](value: Tree[A])(func: A => Tree[B]): Tree[B] = value match {
      case Branch(left, right) => Branch(flatMap(left)(func), flatMap(right)(func))
      case Leaf(v) => func(v)
    }

  }

}
