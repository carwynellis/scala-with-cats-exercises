package scalawithcats.exercises.chapter04

object TreeMonad {

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)
  def leaf[A](value: A): Tree[A] = Leaf(value)

  val treeMonad = new cats.Monad[Tree] {
    override def pure[A](a: A): Tree[A] = Leaf(a)

    // This is not tail recursive and thus not stack safe.
    override def flatMap[A, B](value: Tree[A])(func: A => Tree[B]): Tree[B] = value match {
      case Branch(left, right) => Branch(flatMap(left)(func), flatMap(right)(func))
      case Leaf(v) => func(v)
    }

    // TODO - this is not stack safe.
    // See for more info on tailrecm in cats. https://typelevel.org/cats/faq.html#tailrecm
    // Note - that a left signals further recursion, a right the last step of recursion.
    override def tailRecM[A, B](arg: A)(func: A => Tree[Either[A, B]]): Tree[B] = {
      func(arg) match {
        case Branch(left, right) => Branch(
          flatMap(left) {
            case Left(value) => tailRecM(value)(func)
            case Right(value) => pure(value)
          },
          flatMap(right) {
            case Left(value) => tailRecM(value)(func)
            case Right(value) => pure(value)
          }
        )
        case Leaf(Left(value)) => tailRecM(value)(func)
        case Leaf(Right(value)) => pure(value)
      }
    }
  }

}
