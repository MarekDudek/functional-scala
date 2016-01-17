package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.Folding.{foldLeft2, foldRight2, reverse, reverse2}
import org.scalatest.{FlatSpec, Matchers}

class FoldingSpec extends FlatSpec with Matchers {

  "foldRight via foldLeft" should "work" in {
    foldLeft2(FList(1, 2, 3, 4, 5), FNil: FList[Int])((acc, a) => Cons(a, acc)) shouldBe FList(5, 4, 3, 2, 1)
  }

  "foldLeft via foldRight" should "work" in {
    foldRight2(FList(1, 2, 3, 4, 5), FNil: FList[Int])((a, acc) => Cons(a, acc)) shouldBe FList(5, 4, 3, 2, 1)
  }

  "reverse on empty list" should "return the same" in {
    reverse(FNil) shouldBe FNil
  }

  "reverse on 1 to 5" should "return 5 to 1" in {
    reverse(FList(1, 2, 3, 4, 5)) shouldBe FList(5, 4, 3, 2, 1)
  }

  "reverse2 on empty list" should "return the same" in {
    reverse2(FNil) shouldBe FNil
  }

  "reverse2 on 1 to 5" should "return 5 to 1" in {
    reverse2(FList(1, 2, 3, 4, 5)) shouldBe FList(5, 4, 3, 2, 1)
  }
}
