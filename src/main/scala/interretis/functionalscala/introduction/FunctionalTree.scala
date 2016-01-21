package interretis.functionalscala.introduction

sealed trait FTree[+A]

case class Leaf[A](value: A) extends FTree[A]

case class Branch[A](left: FTree[A], right: FTree[A]) extends FTree[A]


object FunctionalTree {

  def treeSize[A](t: FTree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + treeSize(l) + treeSize(r)
    }

  def treeSize2[A](t: FTree[A]): Int =
    fold(t)(_ => 1)((l, r) => 1 + l + r)

  def maximum(t: FTree[Int]): Int =
    t match {
      case Leaf(n) => n
      case Branch(l, r) => maximum(l) max maximum(r)
    }

  def maximum2(t: FTree[Int]): Int =
    fold(t)(identity)((l, r) => l max r)

  def depth[A](t: FTree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) max depth(r))
    }

  def depth2[A](t: FTree[A]): Int =
    fold(t)(_ => 0)((l, r) => 1 + (l max r))

  def mapTree[A, B](t: FTree[A], f: A => B): FTree[B] =
    t match {
      case Leaf(n) => Leaf(f(n))
      case Branch(l, r) => Branch(mapTree(l, f), mapTree(r, f))
    }

  def mapTree2[A, B](t: FTree[A], f: A => B): FTree[B] =
    fold(t)(a => Leaf(f(a)): FTree[B])((l: FTree[B], r: FTree[B]) => Branch(l, r))

  def fold[A, B](t: FTree[A])(f: A => B)(m: (B, B) => B): B =
    t match {
      case Leaf(n) => f(n)
      case Branch(l, r) => m(fold(l)(f)(m), fold(r)(f)(m))
    }
}
