package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FEither.{attempt, sequence, traverse}
import org.scalatest.{FlatSpec, Matchers}

class FEitherSpec extends FlatSpec with Matchers {

  "sequence with errors" should "return first error" in {
    sequence(List(FRight(1), FLeft("two"), FRight(3), FLeft("four"), FRight(5))) shouldBe FLeft("two")
  }

  "sequence without errors" should "return all results" in {
    sequence(List(FRight(1), FRight(2), FRight(3), FRight(4), FRight(5))) shouldBe FRight(List(1, 2, 3, 4, 5))
  }

  "traverse with errors" should "return first error" in {
    traverse(List("1", "two", "3", "four", "5"))((s: String) => attempt(s.toInt)) shouldBe FLeft("""For input string: "two"""")
  }

  "traverse without errors" should "return all results" in {
    traverse(List("1", "2", "3", "4", "5"))((s: String) => attempt(s.toInt)) shouldBe FRight(List(1, 2, 3, 4, 5))
  }
}
