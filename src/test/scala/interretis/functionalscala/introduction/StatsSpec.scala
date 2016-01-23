package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FOption.{attempt, sequence, sequence2, sequence3, traverse}
import interretis.functionalscala.introduction.Stats.{abs, arithmeticMean, insuranceRateQuote, insuranceRateQuote2, variance}
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

  "abs" should "work" in {
    val v = variance(Seq(3, 4, 7, 10))
    abs(v) shouldBe FSome(7.5)
    v.map(math.abs) shouldBe FSome(7.5)
  }

  "sequence with None" should "return None" in {
    sequence(List(FSome(1), FSome(2), FNone, FSome(4))) shouldBe FNone
  }

  "sequence without None" should "return Some" in {
    sequence(List(FSome(1), FSome(2), FSome(3), FSome(4))) shouldBe FSome(List(1, 2, 3, 4))
  }

  "sequence (2) with None" should "return None" in {
    sequence2(List(FSome(1), FSome(2), FNone, FSome(4))) shouldBe FNone
  }

  "sequence (2) without None" should "return Some" in {
    sequence2(List(FSome(1), FSome(2), FSome(3), FSome(4))) shouldBe FSome(List(1, 2, 3, 4))
  }

  "traverse" should "work with simple example" in {
    traverse(List("1", "2", "3"))(s => attempt(s.toInt)) shouldBe FSome(List(1, 2, 3))
  }

  "traverse" should "work with simple example, fail case" in {
    traverse(List("1", "two", "3"))(s => attempt(s.toInt)) shouldBe FNone
  }

  "sequence (3) with None" should "return None" in {
    sequence3(List(FSome(1), FSome(2), FNone, FSome(4))) shouldBe FNone
  }

  "sequence (3) without None" should "return Some" in {
    sequence3(List(FSome(1), FSome(2), FSome(3), FSome(4))) shouldBe FSome(List(1, 2, 3, 4))
  }

  "insurance quote" should "work fine" in {
    insuranceRateQuote(2, 5) shouldBe 2.5
  }

  "unsure insurance quote" should "work fine" in {
    insuranceRateQuote("2", "5") shouldBe FSome(2.5)
  }

  "unsure insurance quote (2)" should "work fine" in {
    insuranceRateQuote2("2", "5") shouldBe FSome(2.5)
  }
}
