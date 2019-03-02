package scalawithcats.exercises.chapter11

final case class GCounter(counters: Map[String, Int]) {

  def increment(machine: String, amount: Int): GCounter =
    GCounter(counters.updated(machine, counters.get(machine).fold(amount)(_ + amount)))

  def merge(that: GCounter): GCounter = GCounter(
    counters.keySet.union(that.counters.keySet).map { machine =>
      machine -> counters.getOrElse(machine, 0).max(that.counters.getOrElse(machine, 0))
    }.toMap
  )

  def total: Int = counters.values.sum

}
