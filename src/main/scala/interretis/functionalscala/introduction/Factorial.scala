package interretis.functionalscala.introduction

import scala.annotation.tailrec

object Factorial {

  def factorial(n: Int): Int = {
    @tailrec
    def factorial(n: Int, acc: Int): Int =
      if (n <= 0)
        acc
      else
        factorial(n - 1, acc * n)
    factorial(n, 1)
  }

  def main(args: Array[String]) {
    assert(factorial(5) == 120)
  }
}
