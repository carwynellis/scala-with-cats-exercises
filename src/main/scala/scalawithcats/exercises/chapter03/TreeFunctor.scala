package scalawithcats.exercises.chapter03

import cats._

object TreeFunctor {

  implicit def treeFunctor = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(v) => Leaf(f(v))
    }
  }
}
