package interretis.functionalscala.introduction

import scala.annotation.tailrec

object PolymorphicFunction {

  def findFirst[A](ss: Array[A], p: A => Boolean): Int = {
    @tailrec
    def loop(n: Int): Int =
      if (n >= ss.length) -1
      else if (p(ss(n))) n
      else loop(n + 1)
    loop(0)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(i: Int): Boolean =
      if (i >= as.length - 1) true
      else if (gt(as(i), as(i + 1))) false
      else loop(i + 1)
    loop(0)
  }
}
