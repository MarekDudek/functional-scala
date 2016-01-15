package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FList.{init2, drop, dropWhile, init}
import org.scalatest.{FlatSpec, Matchers}

class FunctionalListSpec extends FlatSpec with Matchers {

  private val five = Cons(5, FNil)
  private val fourToFive = Cons(4, five)
  private val threeToFive = Cons(3, fourToFive)
  private val twoToFive = Cons(2, threeToFive)
  private val oneToFive = Cons(1, twoToFive)

  "dropping from empty list" should "return empty list" in {
    drop(FNil, 0) shouldBe FNil
  }

  "dropping one" should "work" in {
    drop(oneToFive, 1) shouldBe twoToFive
  }

  "dropping two" should "work" in {
    drop(oneToFive, 2) shouldBe threeToFive
  }

  "dropping three" should "work" in {
    drop(oneToFive, 3) shouldBe fourToFive
  }

  "dropping four" should "work" in {
    drop(oneToFive, 4) shouldBe five
  }

  "dropping all" should "return empty list" in {
    drop(oneToFive, 5) shouldBe FNil
  }

  "dropping more that length" should "not be an error" in {
    drop(oneToFive, 6) shouldBe FNil
  }

  "dropWhile 1" should "work" in {
    dropWhile(oneToFive, (n: Int) => n > 6) shouldBe oneToFive
  }

  "dropWhile 2" should "work" in {
    dropWhile(oneToFive, (n: Int) => n < 2) shouldBe twoToFive
  }

  "dropWhile 3" should "work" in {
    dropWhile(oneToFive, (n: Int) => n < 5) shouldBe five
  }

  "dropWhile 4" should "work" in {
    dropWhile(oneToFive, (n: Int) => n % 2 != 0) shouldBe twoToFive
  }

  private val oneToFour = Cons(1, Cons(2, Cons(3, Cons(4, FNil))))
  private val oneToThree = Cons(1, Cons(2, Cons(3, FNil)))
  private val oneToTwo = Cons(1, Cons(2, FNil))
  private val one = Cons(1, FNil)

  "init 1" should "work" in {
    init(oneToFive) shouldBe oneToFour
  }

  "init 2" should "work" in {
    init(oneToFour) shouldBe oneToThree
  }

  "init 3" should "work" in {
    init(oneToThree) shouldBe oneToTwo
  }

  "init 4" should "work" in {
    init(oneToTwo) shouldBe one
  }

  "init 5" should "work" in {
    init(one) shouldBe FNil
  }


  "init2 1" should "work" in {
    init2(oneToFive) shouldBe oneToFour
  }

  "init2 2" should "work" in {
    init2(oneToFour) shouldBe oneToThree
  }

  "init2 3" should "work" in {
    init2(oneToThree) shouldBe oneToTwo
  }

  "init2 4" should "work" in {
    init2(oneToTwo) shouldBe one
  }

  "init2 5" should "work" in {
    init2(one) shouldBe FNil
  }
}
