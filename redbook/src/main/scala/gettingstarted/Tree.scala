package gettingstarted

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


/** REPL DATA (for testing)
 *
 * scala> Tree.size(my.tree)
 * res0: Int = 7
 *
 */
object my {
  var tree = Branch(Branch(Leaf(1),Leaf(2)), Branch(Leaf(3),Leaf(4)))
  var xtree = Branch(Branch(Leaf(1),Leaf(2)), Branch(Branch(Leaf(3),Leaf(4)),Leaf(0)))
}


object Tree {

  /** 3.25
   *
   * scala> Tree.size(my.tree)
   * res0: Int = 7
   *
   * scala> Tree.numLeaves(my.tree)
   * res1: Int = 4
   */
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left,right) => 1 + size(left) + size(right)
  }

  def numLeaves[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left,right) => size(left) + size(right)
  }

  /** 3.26
   *
   * scala> Tree.maximum(my.tree)
   * res0: Int = 4
   *
   */
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(left,right) => maximum(left) max maximum(right)
  }

  /** 3.27
   *
   * scala> Tree.depth(my.tree)
   * res0: Int = 2
   *
   */
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left,right) => 1 + (depth(left) max depth(right))
  }

  /** 3.28
   *
   * scala> Tree.map(my.tree)(_+1.5)
   * res0: Tree[Double] = Branch(Branch(Leaf(2.5),Leaf(3.5)),Branch(Leaf(4.5),Leaf(5.5)))
   *
   */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left,right) => Branch(map(left)(f), map(right)(f))
  }

  /** 3.29
   *
   * scala> Tree.fold(my.tree)(a => Leaf(a+1.5): Tree[Double])(Branch(_,_))
   * res0: Tree[Double] = Branch(Branch(Leaf(2.5),Leaf(3.5)),Branch(Leaf(4.5),Leaf(5.5)))
   *
   */
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(left,right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  /**
   *
   * scala> Tree.sizeViaFold(my.tree)
   * res0: Int = 7
   */
  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(1 + _ + _)

  /**
   *
   * scala> Tree.numLeavesViaFold(my.tree)
   * res0: Int = 4
   */
  def numLeavesViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 1)(_ + _)

  /**
   *
   * scala> Tree.maximumViaFold(my.tree)
   * res0: Int = 4
   */
  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)(_ max _)

  /**
   *
   * scala> Tree.depthViaFold(my.tree)
   * res0: Int = 2
   */
  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(a => 0)((d1,d2) => 1 + (d1 max d2))

  /**
   *
   * scala> Tree.mapViaFold(my.tree)(_+0.5)
   * res0: Tree[Double] = Branch(Branch(Leaf(1.5),Leaf(2.5)),Branch(Leaf(3.5),Leaf(4.5)))
   */
  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch(_,_))
}
