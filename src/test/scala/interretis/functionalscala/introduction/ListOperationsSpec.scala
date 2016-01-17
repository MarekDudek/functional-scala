package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.ListOperations.{append, append2, append3, length2, init, init2}
import org.scalatest.{FlatSpec, Matchers}

class ListOperationsSpec extends FlatSpec with Matchers {

  private val l1to5 = FList(1, 2, 3, 4, 5)
  private val l1to4 = FList(1, 2, 3, 4)
  private val l1to3 = FList(1, 2, 3)
  private val l1to2 = FList(1, 2)
  private val l1 = FList(1)

  "length of empty list" should "be 0" in {
    length2(FNil) shouldBe 0
  }

  "length of 1 to 5 list" should "be 5" in {
    length2(l1to5) shouldBe 5
  }

  "init 1" should "work" in {
    init(l1to5) shouldBe l1to4
  }

  "init 2" should "work" in {
    init(l1to4) shouldBe l1to3
  }

  "init 3" should "work" in {
    init(l1to3) shouldBe l1to2
  }

  "init 4" should "work" in {
    init(l1to2) shouldBe l1
  }

  "init 5" should "work" in {
    init(l1) shouldBe FNil
  }


  "init2 1" should "work" in {
    init2(l1to5) shouldBe l1to4
  }

  "init2 2" should "work" in {
    init2(l1to4) shouldBe l1to3
  }

  "init2 3" should "work" in {
    init2(l1to3) shouldBe l1to2
  }

  "init2 4" should "work" in {
    init2(l1to2) shouldBe l1
  }

  "init2 5" should "work" in {
    init2(l1) shouldBe FNil
  }

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
