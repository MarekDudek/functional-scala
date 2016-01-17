package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FList.{drop, dropWhile, foldLeft2, foldRight2, init, init2, reverse, reverse2, append, append2, append3}
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

  "dropping one" should "" in {
    drop(oneToFive, 1) shouldBe twoToFive
  }

  "dropping two" should "" in {
    drop(oneToFive, 2) shouldBe threeToFive
  }

  "dropping three" should "" in {
    drop(oneToFive, 3) shouldBe fourToFive
  }

  "dropping four" should "" in {
    drop(oneToFive, 4) shouldBe five
  }

  "dropping all" should "return empty list" in {
    drop(oneToFive, 5) shouldBe FNil
  }

  "dropping more that length" should "not be an error" in {
    drop(oneToFive, 6) shouldBe FNil
  }

  "dropWhile 1" should "" in {
    dropWhile(oneToFive)(n => n > 6) shouldBe oneToFive
  }

  "dropWhile 2" should "" in {
    dropWhile(oneToFive)(n => n < 2) shouldBe twoToFive
  }

  "dropWhile 3" should "" in {
    dropWhile(oneToFive)(n => n < 5) shouldBe five
  }

  "dropWhile 4" should "" in {
    dropWhile(oneToFive)(n => n % 2 != 0) shouldBe twoToFive
  }

  private val oneToFour = Cons(1, Cons(2, Cons(3, Cons(4, FNil))))
  private val oneToThree = Cons(1, Cons(2, Cons(3, FNil)))
  private val oneToTwo = Cons(1, Cons(2, FNil))
  private val one = Cons(1, FNil)

  "init 1" should "" in {
    init(oneToFive) shouldBe oneToFour
  }

  "init 2" should "" in {
    init(oneToFour) shouldBe oneToThree
  }

  "init 3" should "" in {
    init(oneToThree) shouldBe oneToTwo
  }

  "init 4" should "" in {
    init(oneToTwo) shouldBe one
  }

  "init 5" should "" in {
    init(one) shouldBe FNil
  }


  "init2 1" should "" in {
    init2(oneToFive) shouldBe oneToFour
  }

  "init2 2" should "" in {
    init2(oneToFour) shouldBe oneToThree
  }

  "init2 3" should "" in {
    init2(oneToThree) shouldBe oneToTwo
  }

  "init2 4" should "" in {
    init2(oneToTwo) shouldBe one
  }

  "init2 5" should "" in {
    init2(one) shouldBe FNil
  }

  "length of empty list" should "be 0" in {
    FList.length(FNil) shouldBe 0
  }

  "length of one to five list" should "be 5" in {
    FList.length(oneToFive) shouldBe 5
  }

  private val fiveToOne = Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, FNil)))))

  "reverse on empty list" should "return the same" in {
    reverse(FNil) shouldBe FNil
  }

  "reverse on 1 to 5" should "return 5 to 1" in {
    reverse(oneToFive) shouldBe fiveToOne
  }

  "reverse2 on empty list" should "return the same" in {
    reverse2(FNil) shouldBe FNil
  }

  "reverse2 on 1 to 5" should "return 5 to 1" in {
    reverse2(oneToFive) shouldBe fiveToOne
  }

  "foldRight via foldLeft" should "work properly" in {
    foldLeft2(oneToFive, FNil: FList[Int])((acc, a) => Cons(a, acc)) shouldBe fiveToOne
  }

  "foldLeft via foldRight" should "work properly" in {
    foldRight2(oneToFive, FNil: FList[Int])((a, acc) => Cons(a, acc)) shouldBe fiveToOne
  }

  "append to empty list" should "result " in {
    append(FNil, 5) shouldBe five
  }

  "append to non-empty list" should "properly append" in {
    append(oneToFour, 5) shouldBe oneToFive
  }

  "append2 to empty list" should "properly append" in {
    append2(FNil, 5) shouldBe five
  }

  "append2 to non-empty list" should "properly append" in {
    append2(oneToFour, 5) shouldBe oneToFive
  }

  "append3 to empty list" should "properly append" in {
    append3(FNil, 5) shouldBe five
  }

  "append3 to non-empty list" should "properly append" in {
    append3(oneToFour, 5) shouldBe oneToFive
  }
}
