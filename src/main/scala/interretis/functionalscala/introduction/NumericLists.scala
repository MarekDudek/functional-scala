package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.Folding.{foldLeft, foldRight}

object NumericLists {

  def sum(l: FList[Int]): Int =
    l match {
      case FNil => 0
      case Cons(h, t) => h + sum(t)
    }

  def product(l: FList[Double]): Double =
    l match {
      case FNil => 1
      case Cons(h, t) => h * product(t)
    }

  def sum2(l: FList[Int]): Int =
    foldRight(l, 0)(_ + _)

  def product2(l: FList[Int]): Int =
    foldRight(l, 1)(_ * _)

  def sum3(l: FList[Int]): Int =
    foldLeft(l, 0)(_ + _)

  def product3(l: FList[Int]): Int =
    foldLeft(l, 1)(_ * _)
}
