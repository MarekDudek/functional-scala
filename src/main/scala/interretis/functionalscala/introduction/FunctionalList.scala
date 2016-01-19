package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.Folding.{foldLeft, reverse}


sealed trait FList[+A]

case object FNil extends FList[Nothing]

case class Cons[+A](head: A, tail: FList[A]) extends FList[A]


object FList {

  def apply[A](s: A*): FList[A] =
    if (s.isEmpty) FNil
    else Cons(s.head, apply(s.tail: _*))

  def setHead[A](l: FList[A], h: A): FList[A] =
    l match {
      case FNil => sys.error("setHead on empty list")
      case Cons(_, t) => Cons(h, t)
    }

  def addOne(l: FList[Int]): FList[Int] =
    foldLeft(reverse(l), FNil: FList[Int])(
      (t, h) => Cons(h + 1, t)
    )

  def changeToString(l: FList[Int]): FList[String] =
    foldLeft(reverse(l), FNil: FList[String])(
      (t, h) => Cons(h.toString, t)
    )

  def addLists(l1: FList[Int], l2: FList[Int]): FList[Int] =
    (l1, l2) match {
      case (FNil, _) => FNil
      case (_, FNil) => FNil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    }

  def hasSubsequence[A](list: FList[A], sub: FList[A]): Boolean = {
    def begins(l: FList[A], s: FList[A]): Boolean =
      (l, s) match {
        case (_, FNil) => true
        case (Cons(h1, t1), Cons(h2, t2)) =>
          h1 == h2 && begins(t1, t2)
        case _ => false
      }
    (list, sub) match {
      case (_, FNil) => true
      case (Cons(h1, t1), Cons(h2, t2)) =>
        if (h1 == h2 && begins(t1, t2)) true
        else hasSubsequence(t1, sub)
      case _ => false
    }
  }
}
