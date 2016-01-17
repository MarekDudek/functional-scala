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
      (acc, a) => Cons(a + 1, acc)
    )

  def changeToString(is: FList[Int]): FList[String] =
    foldLeft(reverse(is), FNil: FList[String])(
      (acc, i) => Cons(i.toString, acc)
    )

  def addLists(l1: FList[Int], l2: FList[Int]): FList[Int] =
    (l1, l2) match {
      case (FNil, _) => FNil
      case (_, FNil) => FNil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
    }
}
