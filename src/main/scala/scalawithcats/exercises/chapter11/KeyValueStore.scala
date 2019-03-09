package scalawithcats.exercises.chapter11

import scala.language.higherKinds

trait KeyValueStore[F[_,_]] {

  def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

  def get[K, V](f: F[K, V])(k: K): Option[V]

  def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).getOrElse(default)

  def values[K, V](f: F[K, V]): List[V]

}

object KeyValueStore {

  implicit def mapInstance = new KeyValueStore[Map] {

    override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f.updated(k, v)

    override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

    override def values[K, V](f: Map[K, V]): List[V] = f.values.toList

  }

}
