package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.Folding.{foldLeft, reverse}
import interretis.functionalscala.introduction.ListOperations.concat

object HigherOrderFunctions {

  def map[A, B](l: FList[A])(f: A => B): FList[B] =
    foldLeft(reverse(l), FNil: FList[B])(
      (acc, a) => Cons(f(a), acc)
    )

  def flatMap[A, B](l: FList[A])(f: A => FList[B]): FList[B] =
    foldLeft(reverse(l), FNil: FList[B])(
      (acc, a) => concat(f(a), acc)
    )

  def filter[A](l: FList[A])(p: A => Boolean): FList[A] =
    foldLeft(reverse(l), FNil: FList[A])(
      (acc, h) => if (p(h)) Cons(h, acc) else acc
    )

  def filter2[A](l: FList[A])(p: A => Boolean): FList[A] =
    flatMap(l)(
      a => if (p(a)) Cons(a, FNil) else FNil
    )

  def zipWith[A, B, C](l1: FList[A], l2: FList[B])(f: (A, B) => C): FList[C] =
    (l1, l2) match {
      case (FNil, _) => FNil
      case (_, FNil) => FNil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
    }
}
