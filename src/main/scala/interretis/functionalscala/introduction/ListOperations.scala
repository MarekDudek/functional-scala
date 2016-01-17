package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.Folding.{foldLeft, foldRight, reverse}

object ListOperations {

  def length2[A](l: FList[A]): Int =
    foldRight(l, 0)((_, len) => len + 1)

  def length3[A](l: FList[A]): Int =
    foldLeft(l, 0)((len, _) => len + 1)

  def init[A](l: FList[A]): FList[A] =
    l match {
      case FNil => sys.error("init on empty list")
      case Cons(_, FNil) => FNil
      case Cons(h, t) => Cons(h, init(t))
    }

  def init2[A](l: FList[A]): FList[A] = {
    val buffer = scala.collection.mutable.ListBuffer[A]()
    def loop(rest: FList[A]): FList[A] =
      rest match {
        case FNil => sys.error("init on empty list")
        case Cons(_, FNil) => FList(buffer.toList: _*)
        case Cons(h, t) => buffer += h; loop(t)
      }
    loop(l)
  }

  def tail[A](l: FList[A]): FList[A] =
    l match {
      case FNil => sys.error("tail on empty list")
      case Cons(_, t) => t
    }

  def drop[A](l: FList[A], n: Int): FList[A] = {
    if (n <= 0) l
    else l match {
      case FNil => FNil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  def dropWhile[A](l: FList[A])(p: A => Boolean): FList[A] =
    l match {
      case Cons(h, t) if p(h) => dropWhile(t)(p)
      case _ => l
    }

  def concat[A](l1: FList[A], l2: FList[A]): FList[A] =
    foldRight(l1, l2)((h1, h2) => Cons(h1, h2))

  def flatten[A](l: FList[FList[A]]): FList[A] =
    foldRight(l, FNil: FList[A])((h1, h2) => concat(h1, h2))

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
