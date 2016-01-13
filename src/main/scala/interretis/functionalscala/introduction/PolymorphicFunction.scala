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
}
