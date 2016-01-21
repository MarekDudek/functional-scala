package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.Stats.{abs, arithmeticMean, variance, variance2}
import org.scalatest.{FlatSpec, Matchers}

class StatsSpec extends FlatSpec with Matchers {

  "mean of empty list" should "be None" in {
    arithmeticMean(Seq()) shouldBe FNone
  }

  "mean of example from Wikipedia" should "be correct" in {
    arithmeticMean(Seq(4, 36, 45, 50, 75)) shouldBe FSome(42.0)
  }

  "variance of empty list" should "be None" in {
    variance(Seq()) shouldBe FNone
  }

  "variance of example from Wikipedia" should "be correct" in {
    variance(Seq(3, 4, 7, 10)) shouldBe FSome(7.5)
  }

  "variance (2) of empty list" should "be None" in {
    variance2(Seq()) shouldBe FNone
  }

  "variance (2) of example from Wikipedia" should "be correct" in {
    variance2(Seq(3, 4, 7, 10)) shouldBe FSome(7.5)
  }

  "abs" should "work" in {
    val variance = variance2(Seq(3, 4, 7, 10))
    abs(variance) shouldBe FSome(7.5)
    variance.map(math.abs) shouldBe FSome(7.5)
  }
}
