package interretis.functionalscala.introduction

import interretis.functionalscala.introduction.FunctionalTree.{depth, depth2, mapTree, mapTree2, maximum, maximum2, treeSize, treeSize2}
import org.scalatest.{FlatSpec, Matchers}

class FunctionalTreeSpec extends FlatSpec with Matchers {

  private val leaf = Leaf(5)
  private val tree = Branch(leaf, Branch(Leaf(3), Leaf(7)))

  "leaf" should "be of size 1" in {
    treeSize(leaf) shouldBe 1
  }

  "tree" should "be of size 5" in {
    treeSize(tree) shouldBe 5
  }

  "leaf" should "be of size 1 (2)" in {
    treeSize2(leaf) shouldBe 1
  }

  "tree" should "be of size 5 (2)" in {
    treeSize2(tree) shouldBe 5
  }

  "leaf" should "max at 5" in {
    maximum(leaf) shouldBe 5
  }

  "tree" should "max at 7" in {
    maximum(tree) shouldBe 7
  }

  "leaf" should "max at 5 (2)" in {
    maximum2(leaf) shouldBe 5
  }

  "tree" should "max at 7 (2)" in {
    maximum2(tree) shouldBe 7
  }

  "leaf" should "be 0 deep" in {
    depth(leaf) shouldBe 0
  }

  "tree" should "be 2 deep" in {
    depth(tree) shouldBe 2
  }

  "leaf" should "be 0 deep (2)" in {
    depth2(leaf) shouldBe 0
  }

  "tree" should "be 2 deep (2)" in {
    depth2(tree) shouldBe 2
  }

  private val doubled = (n: Int) => 2 * n

  "leaf" should "map properly" in {
    mapTree(leaf, doubled) shouldBe Leaf(10)
  }

  "tree" should "map properly" in {
    mapTree(tree, doubled) shouldBe Branch(Leaf(10), Branch(Leaf(6), Leaf(14)))
  }

  "leaf" should "map properly (2)" in {
    mapTree2(leaf, doubled) shouldBe Leaf(10)
  }

  "tree" should "map properly (2)" in {
    mapTree2(tree, doubled) shouldBe Branch(Leaf(10), Branch(Leaf(6), Leaf(14)))
  }
}
