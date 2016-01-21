package interretis.functionalscala.introduction

sealed trait FTree[+A]

case class Leaf[A](value: A) extends FTree[A]

case class Branch[A](left: FTree[A], right: FTree[A]) extends FTree[A]


object FunctionalTree {

  def treeSize[A](tree: FTree[A]): Int =
    tree match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + treeSize(l) + treeSize(r)
    }

  def maximum(tree: FTree[Int]): Int =
    tree match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def depth[A](tree: FTree[A]): Int =
    tree match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def mapTree[A, B](tree: FTree[A], f: A => B): FTree[B] =
    tree match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(mapTree(l, f), mapTree(r, f))
    }
}
