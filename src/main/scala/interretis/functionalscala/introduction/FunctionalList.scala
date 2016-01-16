package interretis.functionalscala.introduction

import scala.annotation.tailrec


sealed trait FList[+A]

case object FNil extends FList[Nothing]

case class Cons[+A](head: A, tail: FList[A]) extends FList[A]

object FList {

  def sum(ints: FList[Int]): Int = ints match {
    case FNil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: FList[Double]): Double = ds match {
    case FNil => 1.0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](as: FList[A], z: B)(f: (A, B) => B): B =
    as match {
      case FNil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  @tailrec
  def foldLeft[A, B](as: FList[A], z: B)(f: (B, A) => B): B =
    as match {
      case FNil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sum2(ints: FList[Int]): Int =
    foldRight(ints, 0)(_ + _)

  def product2(ints: FList[Int]): Int =
    foldRight(ints, 1)(_ * _)

  def length[A](as: FList[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  def sum3(ints: FList[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product3(ints: FList[Int]): Int =
    foldLeft(ints, 1)(_ * _)

  def length3[A](as: FList[A]): Int =
    foldLeft(as, 0)((acc, _) => acc + 1)

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

  def init[A](list: FList[A]): FList[A] =
    list match {
      case FNil => sys.error("init on empty list")
      case Cons(_, FNil) => FNil
      case Cons(x, xs) => Cons(x, init(xs))
    }

  def init2[A](list: FList[A]): FList[A] = {
    val buffer = scala.collection.mutable.ListBuffer[A]()
    def loop(rest: FList[A]): FList[A] =
      rest match {
        case FNil => sys.error("init on empty list")
        case Cons(_, FNil) => FList(buffer.toList: _*)
        case Cons(x, xs) => buffer += x; loop(xs)
      }
    loop(list)
  }
}
