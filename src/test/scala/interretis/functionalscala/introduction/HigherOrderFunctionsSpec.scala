package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.HigherOrderFunctions.{filter, filter2, map}
import org.scalatest.{FlatSpec, Matchers}

class HigherOrderFunctionsSpec extends FlatSpec with Matchers {

  "map" should "work" in {
    map(FList(1, 2, 3, 4, 5))(_.toString) shouldBe FList("1", "2", "3", "4", "5")
  }

  "filter" should "work" in {
    filter(FList(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe FList(2, 4)
  }

  "filter2" should "work" in {
    filter2(FList(1, 2, 3, 4, 5))(_ % 2 == 0) shouldBe FList(2, 4)
  }
}
