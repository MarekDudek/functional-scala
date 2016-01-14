package interretis.functionalscala.introduction


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
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): FList[A] =
    if (as.isEmpty)
      FNil
    else
      Cons(as.head, apply(as.tail: _*))
}
