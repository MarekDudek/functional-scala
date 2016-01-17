package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.ListOperations.{append, append2, append3}
import org.scalatest.{FlatSpec, Matchers}

class ListOperationsSpec extends FlatSpec with Matchers {

  private val l1to5 = FList(1, 2, 3, 4, 5)
  private val l1to4 = FList(1, 2, 3, 4)

  "append to empty list" should "work" in {
    append(FNil, 5) shouldBe FList(5)
  }

  "append to non-empty list" should "work" in {
    append(l1to4, 5) shouldBe l1to5
  }

  "append2 to empty list" should "work" in {
    append2(FNil, 5) shouldBe FList(5)
  }

  "append2 to non-empty list" should "work" in {
    append2(l1to4, 5) shouldBe l1to5
  }

  "append3 to empty list" should "work" in {
    append3(FNil, 5) shouldBe FList(5)
  }

  "append3 to non-empty list" should "work" in {
    append3(l1to4, 5) shouldBe l1to5
  }
}
