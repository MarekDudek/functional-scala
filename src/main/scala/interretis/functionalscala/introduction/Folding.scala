package interretis.functionalscala.introduction

import scala.annotation.tailrec

object Folding {

  def foldRight[A, B](l: FList[A], z: B)(f: (A, B) => B): B =
    l match {
      case FNil => z
      case Cons(h, t) => f(h, foldRight(t, z)(f))
    }

  @tailrec
  def foldLeft[A, B](l: FList[A], z: B)(f: (B, A) => B): B =
    l match {
      case FNil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  def foldLeft2[A, B](l: FList[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((h, acc) => f(acc, h))

  def foldRight2[A, B](l: FList[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, z)((acc, h) => f(h, acc))

  def reverse[A](l: FList[A]): FList[A] = {
    @tailrec
    def loop(src: FList[A], acc: FList[A]): FList[A] =
      src match {
        case FNil => acc
        case Cons(h, t) => loop(t, Cons(h, acc))
      }
    loop(l, FNil)
  }

  def reverse2[A](l: FList[A]): FList[A] =
    foldLeft(l, FNil: FList[A])((acc, h) => Cons(h, acc))
}
