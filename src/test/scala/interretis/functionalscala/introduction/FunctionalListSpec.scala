package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FList._
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
    drop(FList(1, 2, 3, 4, 5), 1) shouldBe FList(2, 3, 4, 5)
  }

  "dropping two" should "" in {
    drop(FList(1, 2, 3, 4, 5), 2) shouldBe FList(3, 4, 5)
  }

  "dropping three" should "" in {
    drop(FList(1, 2, 3, 4, 5), 3) shouldBe FList(4, 5)
  }

  "dropping four" should "" in {
    drop(FList(1, 2, 3, 4, 5), 4) shouldBe FList(5)
  }

  "dropping all" should "return empty list" in {
    drop(FList(1, 2, 3, 4, 5), 5) shouldBe FNil
  }

  "dropping more that length" should "not be an error" in {
    drop(FList(1, 2, 3, 4, 5), 6) shouldBe FNil
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




  private val fiveToOne = Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, FNil)))))


  "concat" should "work" in {
    concat(oneToThree, fourToFive) shouldBe oneToFive
  }

  "add one" should "work" in {
    addOne(oneToFour) shouldBe twoToFive
  }

  private val oneToFiveStrings = Cons("1", Cons("2", Cons("3", Cons("4", Cons("5", FNil)))))

  "int to string" should "work" in {
    changeToString(oneToFive) shouldBe oneToFiveStrings
  }

  "map" should "work" in {
    map(oneToFive)(_.toString) shouldBe oneToFiveStrings
  }

  "filter" should "work" in {
    filter(oneToFive)(_ % 2 == 0) shouldBe Cons(2, Cons(4, FNil))
  }

  "filter2" should "work" in {
    FList2.filter2(oneToFive)(_ % 2 == 0) shouldBe Cons(2, Cons(4, FNil))
  }

  "add lists" should "work" in {
    FList2.addLists(Cons(1, Cons(2, Cons(3, FNil))), Cons(4, Cons(5, Cons(6, FNil)))) shouldBe Cons(5, Cons(7, Cons(9, FNil)))
  }
}
