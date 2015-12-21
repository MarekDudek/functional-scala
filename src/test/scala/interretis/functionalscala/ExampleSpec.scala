package interretis.functionalscala

import org.scalatest.{FlatSpec, Matchers}

class ExampleSpec extends FlatSpec with Matchers {

  "test" should "work" in {
    2 + 2 shouldBe 4
  }
}
