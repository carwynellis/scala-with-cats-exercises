package scalawithcats.exercises.chapter09

import cats.Monoid
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object FoldMap {

  def foldMap[A,B : Monoid](v: Vector[A])(f: A => B): B =
    v.map(f).fold(Monoid[B].empty)(Monoid[B].combine)

  private val Processors = Runtime.getRuntime.availableProcessors()

  def parallelFoldMap[A, B : Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val groupSize = math.ceil(values.size.toDouble / Processors).toInt
    val tasks  = values.grouped(groupSize).map(v => Future(foldMap(v)(func)))
    Future.sequence(tasks).map(v => foldMap(v.toVector)(identity))
  }

}
