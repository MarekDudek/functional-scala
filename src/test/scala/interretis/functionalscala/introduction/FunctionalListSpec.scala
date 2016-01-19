package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FList.{addLists, addOne, changeToString, hasSubsequence}
import org.scalatest.{FlatSpec, Matchers}

class FunctionalListSpec extends FlatSpec with Matchers {

  "add one" should "work" in {
    addOne(FList(1, 2, 3, 4)) shouldBe FList(2, 3, 4, 5)
  }

  "int to string" should "work" in {
    changeToString(FList(1, 2, 3, 4, 5)) shouldBe FList("1", "2", "3", "4", "5")
  }

  "add lists" should "work" in {
    addLists(FList(1, 2, 3, 4), FList(4, 5, 6)) shouldBe FList(5, 7, 9)
  }

  "hasSubsequence" should "work on complicated example" in {
    hasSubsequence(FList(2, 1, 3, 1, 2, 4, 1, 2, 3, 4), FList(1, 2, 3)) shouldBe true
  }

  "hasSubsequence" should "work for two empty lists" in {
    hasSubsequence(FNil, FNil) shouldBe true
  }

  "hasSubsequence" should "work when sublist is empty" in {
    hasSubsequence(FList(1), FNil) shouldBe true
  }

  "hasSubsequence" should "work when whole equal to part" in {
    hasSubsequence(FList(2, 1, 3, 1, 2, 4, 1, 2, 3, 4), FList(2, 1, 3, 1, 2, 4, 1, 2, 3, 4)) shouldBe true
  }

  "hasSubsequence" should "work when whole begins with part" in {
    hasSubsequence(FList(1, 2, 3, 1, 2, 4, 1, 2, 3, 4), FList(1, 2, 3)) shouldBe true
  }

  "hasSubsequence" should "work when whole ends with part" in {
    hasSubsequence(FList(1, 2, 1, 2, 4, 1, 1, 2, 3), FList(1, 2, 3)) shouldBe true
  }

  "hasSubsequence" should "not work when no sublist" in {
    hasSubsequence(FList(1, 2, 1, 2, 4, 1, 1, 2, 4, 3), FList(1, 2, 3)) shouldBe false
  }

}
