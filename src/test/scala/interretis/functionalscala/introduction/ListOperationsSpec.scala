package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.ListOperations.{append, append2, append3, concat, drop, dropWhile, init, init2, length2}
import org.scalatest.{FlatSpec, Matchers}

class ListOperationsSpec extends FlatSpec with Matchers {

  "length of empty list" should "be 0" in {
    length2(FNil) shouldBe 0
  }

  "length of 1 to 5 list" should "be 5" in {
    length2(FList(1, 2, 3, 4, 5)) shouldBe 5
  }

  "init 1" should "work" in {
    init(FList(1, 2, 3, 4, 5)) shouldBe FList(1, 2, 3, 4)
  }

  "init 2" should "work" in {
    init(FList(1, 2, 3, 4)) shouldBe FList(1, 2, 3)
  }

  "init 3" should "work" in {
    init(FList(1, 2, 3)) shouldBe FList(1, 2)
  }

  "init 4" should "work" in {
    init(FList(1, 2)) shouldBe FList(1)
  }

  "init 5" should "work" in {
    init(FList(1)) shouldBe FNil
  }

  "init2 1" should "work" in {
    init2(FList(1, 2, 3, 4, 5)) shouldBe FList(1, 2, 3, 4)
  }

  "init2 2" should "work" in {
    init2(FList(1, 2, 3, 4)) shouldBe FList(1, 2, 3)
  }

  "init2 3" should "work" in {
    init2(FList(1, 2, 3)) shouldBe FList(1, 2)
  }

  "init2 4" should "work" in {
    init2(FList(1, 2)) shouldBe FList(1)
  }

  "init2 5" should "work" in {
    init2(FList(1)) shouldBe FNil
  }

  "dropping from empty list" should "return empty list" in {
    drop(FNil, 0) shouldBe FNil
  }

  "dropping one" should "work" in {
    drop(FList(1, 2, 3, 4, 5), 1) shouldBe FList(2, 3, 4, 5)
  }

  "dropping two" should "work" in {
    drop(FList(1, 2, 3, 4, 5), 2) shouldBe FList(3, 4, 5)
  }

  "dropping three" should "work" in {
    drop(FList(1, 2, 3, 4, 5), 3) shouldBe FList(4, 5)
  }

  "dropping four" should "work" in {
    drop(FList(1, 2, 3, 4, 5), 4) shouldBe FList(5)
  }

  "dropping all" should "return empty list" in {
    drop(FList(1, 2, 3, 4, 5), 5) shouldBe FNil
  }

  "dropping more that length" should "not be an error" in {
    drop(FList(1, 2, 3, 4, 5), 6) shouldBe FNil
  }

  "dropWhile 1" should "work" in {
    dropWhile(FList(1, 2, 3, 4, 5))(n => n > 6) shouldBe FList(1, 2, 3, 4, 5)
  }

  "dropWhile 2" should "work" in {
    dropWhile(FList(1, 2, 3, 4, 5))(n => n < 2) shouldBe FList(2, 3, 4, 5)
  }

  "dropWhile 3" should "work" in {
    dropWhile(FList(1, 2, 3, 4, 5))(n => n < 5) shouldBe FList(5)
  }

  "dropWhile 4" should "work" in {
    dropWhile(FList(1, 2, 3, 4, 5))(n => n % 2 != 0) shouldBe FList(2, 3, 4, 5)
  }

  "concat" should "work" in {
    concat(FList(1, 2, 3), FList(4, 5)) shouldBe FList(1, 2, 3, 4, 5)
  }

  "append to empty list" should "work" in {
    append(FNil, 5) shouldBe FList(5)
  }

  "append to non-empty list" should "work" in {
    append(FList(1, 2, 3, 4), 5) shouldBe FList(1, 2, 3, 4, 5)
  }

  "append2 to empty list" should "work" in {
    append2(FNil, 5) shouldBe FList(5)
  }

  "append2 to non-empty list" should "work" in {
    append2(FList(1, 2, 3, 4), 5) shouldBe FList(1, 2, 3, 4, 5)
  }

  "append3 to empty list" should "work" in {
    append3(FNil, 5) shouldBe FList(5)
  }

  "append3 to non-empty list" should "work" in {
    append3(FList(1, 2, 3, 4), 5) shouldBe FList(1, 2, 3, 4, 5)
  }
}
