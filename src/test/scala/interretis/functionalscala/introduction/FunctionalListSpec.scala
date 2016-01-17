package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FList.{addLists, addOne, changeToString}
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
}
