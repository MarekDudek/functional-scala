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
