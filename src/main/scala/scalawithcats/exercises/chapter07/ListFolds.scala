package scalawithcats.exercises.chapter07

object ListFolds {

  def foldListLeft[A](l: List[A]) = l.foldLeft(List.empty[A]){ (acc, elem) => elem :: acc }

  def foldListRight[A](l: List[A]) = l.foldRight(List.empty[A]){ (elem, acc) => elem :: acc }

}
