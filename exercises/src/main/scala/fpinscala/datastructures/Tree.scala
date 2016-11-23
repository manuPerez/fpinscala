package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(a,b) => 1 + size(a) + size(b)
  }

  def maximun(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(x,y) => maximun(x) max maximun(y)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(x,y) => 1 + (depth(x) max depth(y))
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(x,y) => Branch(map(x)(f),map(y)(f))
  }

  def fold[A,B](t: Tree[A])(f: (A) => B)(g: (B,B) => B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(l, r) => g(fold(l)(f)(g),fold(r)(f)(g))
    }

  def sizeViaFold[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

  def maximunViaFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthViaFold[A](t: Tree[A]): Int = fold(t)(a => 0)((t1,t2) => 1 + (t1 max t2))

}


object TestTree{

  import Tree._

  def main(args: Array[String]): Unit = {
    //Exercise 3.25
    println(Tree.size(Branch(Branch(Leaf(1),Branch(Leaf(3),Leaf(5))),Leaf(2))))

    //Exercise 3.26
    println(Tree.maximun(Branch(Branch(Leaf(1),Branch(Leaf(3),Leaf(5))),Leaf(2))))

    //Exercise 3.27
    println(Tree.depth(Branch(Branch(Leaf(1),Branch(Leaf(3),Leaf(5))),Leaf(2))))

    //Exercise 3.28
    println(Tree.map(Branch(Branch(Leaf(1),Branch(Leaf(3),Leaf(5))),Leaf(2)))(x => x + 1))
  }
}