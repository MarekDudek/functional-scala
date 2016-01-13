package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.PolymorphicFunction.isSorted
import org.scalatest.{FlatSpec, Matchers}

class PolymorphicFunctionSpec extends FlatSpec with Matchers {

  private def gt(a: Int, b: Int) = a > b

  "empty array" should "be considered ordered" in {
    isSorted(Array(), gt) shouldBe true
  }

  "one-element array" should "be considered ordered" in {
    isSorted(Array(1), gt) shouldBe true
  }

  "sorted array" should "be sorted" in {
    isSorted((1 to 10).toArray, gt) shouldBe true
  }

  "unsorted array" should "be not sorted" in {
    isSorted(Array(1, 2, 5, 7, 9, 8), gt) shouldBe false
  }
}
