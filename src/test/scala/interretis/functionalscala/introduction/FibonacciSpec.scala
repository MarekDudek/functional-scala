package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.Fibonacci.fib
import org.scalatest.{FlatSpec, Matchers}

class FibonacciSpec extends FlatSpec with Matchers {

  "fib(0)" should "be 0" in {
    fib(0) shouldBe 0
  }

  "fib(1)" should "be 1" in {
    fib(1) shouldBe 1
  }

  "fib(2)" should "be 1" in {
    fib(2) shouldBe 1
  }

  "fib(3)" should "be 2" in {
    fib(3) shouldBe 2
  }

  "fib(4)" should "be 3" in {
    fib(4) shouldBe 3
  }
}
