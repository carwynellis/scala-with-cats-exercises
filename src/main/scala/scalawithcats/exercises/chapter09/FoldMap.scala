package scalawithcats.exercises.chapter09

import cats.Monoid
import cats.implicits._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object FoldMap {

  def foldMap[A,B : Monoid](v: Vector[A])(f: A => B): B =
    v.map(f).fold(Monoid[B].empty)(Monoid[B].combine)

  private val Processors = Runtime.getRuntime.availableProcessors()

  def parallelFoldMap[A, B : Monoid](values: Vector[A])(func: A => B): Future[B] = {
    val tasks  = values.grouped(groupSize(values)).map(v => Future(foldMap(v)(func)))
    Future.sequence(tasks).map(v => foldMap(v.toVector)(identity))
  }

  def parallelFoldMapCats[A, B : Monoid](values: Vector[A])(func: A => B): Future[B] =
    values.grouped(groupSize(values))
      .toList
      .traverse(g => Future(g.foldMap(func)))
      .map(_.combineAll)


  private def groupSize[A](values: Vector[A]) = math.ceil(values.size.toDouble / Processors).toInt

}
