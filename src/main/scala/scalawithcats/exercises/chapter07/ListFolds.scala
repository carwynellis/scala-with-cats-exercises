package scalawithcats.exercises.chapter07

import cats.Monoid

object ListFolds {

  def foldListLeft[A](l: List[A]) = l.foldLeft(List.empty[A]) { (acc, elem) => elem :: acc }

  def foldListRight[A](l: List[A]) = l.foldRight(List.empty[A]) { (elem, acc) => elem :: acc }

  def map[A, B](l: List[A])(f: A => B) = l.foldRight(List.empty[B]) { (elem, acc) => f(elem) :: acc }

  def flatMap[A, B](l: List[A])(f: A => List[B]) = l.foldRight(List.empty[B]) { (elem, acc) => f(elem) ::: acc }

  def filter[A](l: List[A])(p: A => Boolean) = l.foldRight(List.empty[A]) { (elem, acc) =>
    if (p(elem)) elem :: acc
    else acc
  }

  def sum[A](l: List[A])(implicit m: Monoid[A]) = l.foldRight(m.empty) { (elem, acc) => m.combine(elem, acc) }

}
