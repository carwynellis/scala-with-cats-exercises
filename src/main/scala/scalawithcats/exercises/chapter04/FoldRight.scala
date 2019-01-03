package scalawithcats.exercises.chapter04

import cats.Eval

object FoldRight {

  def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): Eval[B] =
    foldRightEval(as, Eval.always(acc))((a, acc) => acc.map(fn(a, _)))

  // Helper that re-implements foldRight in terms of Eval[B] instead of B so we can use defer for stack safety.
  private def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
    case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
    case Nil => acc
  }

}
