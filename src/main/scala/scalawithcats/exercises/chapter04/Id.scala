package scalawithcats.exercises.chapter04

import cats.Id

object Id {
  def pure[A](value: A): Id[A] = value
  def map[A, B](value: Id[A])(func: A => B): Id[B] = func(value)
  def flatMap[A, B](value: Id[A])(func: A => Id[B]): Id[B] = func(value)
}
