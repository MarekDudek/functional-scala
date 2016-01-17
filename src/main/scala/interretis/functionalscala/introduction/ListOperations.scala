package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.Folding.{foldLeft, foldRight, reverse}

object ListOperations {

  def append[A](l: FList[A], e: A): FList[A] =
    l match {
      case FNil => Cons(e, FNil)
      case Cons(h, t) => Cons(h, append(t, e))
    }

  def append2[A](l: FList[A], e: A): FList[A] =
    foldRight(l, Cons(e, FNil))((h, acc) => Cons(h, acc))

  def append3[A](l: FList[A], e: A): FList[A] =
    foldLeft(reverse(l), Cons(e, FNil))((acc, h) => Cons(h, acc))
}
