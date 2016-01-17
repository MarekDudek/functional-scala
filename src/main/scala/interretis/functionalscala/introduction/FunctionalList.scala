package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FList.concat
import interretis.functionalscala.introduction.Folding.{foldLeft, foldRight}


sealed trait FList[+A]

case object FNil extends FList[Nothing]

case class Cons[+A](head: A, tail: FList[A]) extends FList[A]


object FList {


  def concat[A](as: FList[A], bs: FList[A]): FList[A] =
    foldRight(as, bs)((x, y) => Cons(x, y))

  def flatten[A](aas: FList[FList[A]]): FList[A] =
    foldRight(aas, FNil: FList[A])((x, y) => concat(x, y))


  def apply[A](as: A*): FList[A] =
    if (as.isEmpty)
      FNil
    else
      Cons(as.head, apply(as.tail: _*))

  def tail[A](list: FList[A]): FList[A] =
    list match {
      case FNil => sys.error("tail on empty list!")
      case Cons(_, xs) => xs
    }

  def setHead[A](list: FList[A], head: A): FList[A] =
    list match {
      case FNil => sys.error("setHead on empty list!")
      case Cons(x, xs) => Cons(head, xs)
    }

  def drop[A](list: FList[A], n: Int): FList[A] = {
    if (n <= 0)
      list
    else
      list match {
        case FNil => FNil
        case Cons(_, xs) => drop(xs, n - 1)
      }
  }

  def dropWhile[A](list: FList[A])(predicate: A => Boolean): FList[A] =
    list match {
      case Cons(x, xs) if predicate(x) =>
        dropWhile(xs)(predicate)
      case _ =>
        list
    }




  def addOne(is: FList[Int]): FList[Int] =
    foldLeft(Folding.reverse(is), FNil: FList[Int])((acc, a) => Cons(a + 1, acc))

  def changeToString(is: FList[Int]): FList[String] =
    foldLeft(Folding.reverse(is), FNil: FList[String])((acc, i) => Cons(i.toString, acc))

  def map[A, B](as: FList[A])(f: A => B): FList[B] =
    foldLeft(Folding.reverse(as), FNil: FList[B])((acc, a) => Cons(f(a), acc))

  def filter[A](as: FList[A])(f: A => Boolean): FList[A] =
    foldLeft(Folding.reverse(as), FNil: FList[A]) {
      (acc, a) => if (f(a)) Cons(a, acc) else acc
    }
}

object FList2 {

  def flatMap[A, B](as: FList[A])(f: A => FList[B]): FList[B] =
    foldLeft(Folding.reverse(as), FNil: FList[B])((acc, a) => concat(f(a), acc))

  def filter2[A](as: FList[A])(f: A => Boolean): FList[A] =
    flatMap(as)(a => if (f(a)) Cons(a, FNil) else FNil)

  def addLists(as: FList[Int], bs: FList[Int]): FList[Int] =
    (as, bs) match {
      case (FNil, _) => FNil
      case (_, FNil) => FNil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addLists(xs, ys))
    }

  def zipWith[A, B, C](as: FList[A], bs: FList[B])(f: (A, B) => C): FList[C] =
    (as, bs) match {
      case (FNil, _) => FNil
      case (_, FNil) => FNil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }
}
