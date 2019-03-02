package scalawithcats.exercises.chapter11

final case class GCounter(counters: Map[String, Int]) {

  def increment(machine: String, amount: Int): GCounter =
    GCounter(counters.updated(machine, counters.get(machine).fold(amount)(_ + amount)))

  def merge(that: GCounter): GCounter = {
    val merged = counters.keySet.union(that.counters.keySet).map { machine =>
      val thisCount = counters.get(machine).fold(0)(identity)
      val thatCount = that.counters.get(machine).fold(0)(identity)
      if (thisCount > thatCount) machine -> thisCount else machine -> thatCount
    }.toMap
    GCounter(merged)
  }

  def total: Int = counters.values.sum

}
