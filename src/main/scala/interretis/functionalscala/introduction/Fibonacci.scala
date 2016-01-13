package interretis.functionalscala.introduction

import scala.annotation.tailrec

object Fibonacci {

  def fib(n: Int): Int = {
    @tailrec
    def fib(n: Int, prev: Int, cur: Int): Int = {
      if (n == 0) prev
      else fib(n - 1, cur, prev + cur)
    }
    fib(n, 0, 1)
  }
}
