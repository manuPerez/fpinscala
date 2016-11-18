package fpinscala.datastructures


sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("l is empty")
    case Cons(h, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("l is empty")
    case Cons(x, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = n match {
    case 0 => l
    case _ => drop(List.tail(l), n-1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if(f(h)) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("l sis empty")
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acum) => acum + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }
  }

  def sum3(l: List[Int]): Int =
    foldLeft(l, 0)((x,y) => x + y)

  def product3(l: List[Double]): Double =
    foldLeft(l, 1.0)(_*_)

  def length3[A](l: List[A]): Int =
    foldLeft(l, 0)((acum, _) => acum + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((li, h) => Cons(h, li))

  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, foldRight(a2, List[A]())((h, li) => Cons(h, li)))((h2, li2) => Cons(h2, li2))

  def concat[A](li: List[List[A]]): List[A] =
    foldLeft(li, List[A]())((x, xs) => append2(x, xs))

  def transform(l: List[Int]): List[Int] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x + 1, transform(xs))
  }

  def parseToString(l: List[Double]): List[String] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(x.toString, parseToString(xs))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if(f(x)) => Cons(x, filter(xs)(f))
    case Cons(x, xs) if(!f(x)) => filter(xs)(f)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if(f(a)) List(a) else Nil)

  def addPairWise(a1: List[Int], a2: List[Int]): List[Int] = (a1,a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1 + h2), addPairWise(t1, t2))
  }

  def zipWith[A,B,C](a1: List[A], a2: List[B])(f: (A,B) => C): List[C] = (a1,a2) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => false
    case Cons(x, Nil) =>
      sub match {
        case Nil => false
        case Cons(y, Nil) if (y == x) => true
        case Cons(y, Nil) if (y != x) => false
        case Cons(y, ys) if(y == x) => true
      }
    case Cons(x, xs) =>
      sub match {
        case Nil => false
        case Cons(y, Nil) if (y == x) => true
        case Cons(y, Nil) if (y != x) => hasSubsequence(xs, sub)
        case Cons(y, ys) if (y == x) => hasSubsequence(xs, ys)
        case Cons(y, ys) if (y != x) => hasSubsequence(xs, sub)
      }
  }

}

object TestList {

  import List._

  def main(args: Array[String]) {
    //Exercise 3.1: What will be the result of the following match expression?
    //Answer:  3 because is the first pattern that matches with the expression, otherwise it would be 15.
    /*println(x)

    //Exercise 3.2
    //println(List.tail(List(1,2,3,4,5,6)))
    println(List.tail(List(1,Nil)))
    //println(List.tail(List()))

    //Exercise 3.3
    println(List.setHead(List(1,2,3),4))

    //Exercise 3.4
    println(List.drop(List(1,2,3,4,5), 2))

    //Exercise 3.6
    println(List.init(List(1,2,3,4,5)))

    //Exercise 3.9
    println(List.length(List(10,12,13,14,15)))

    //Exercise 3.11
    println(List.sum3(List(1,2,3,4,5)))
    println(List.product3(List(1,2,3,4,5)))
    println(List.length3(List(10,12,13,14,15)))

    //Exercise 3.12
    println(List.reverse(List(1,2,3)))

    //Exercise 3.14
    println(List.append2(List(1,2), List(3,4)))

    //Exercise 3.15
    println(List.concat(List(List(1,2),List(3,4),List(5,6))))

    //Exercise 3.16
    println(List.transform(List(1,2,3,4,5)))

    //Exercise 3.17
    println(List.parseToString(List(1,2,3,4,5)))

    //Exercise 3.18
    println(List.map(List(1,2,3,4,5))(_ + 2))

    //Exercise 3.19
    println(List.filter(List(1,2,3,4,5))(x => x % 2 == 0))

    //Exercise 3.20
    println(List.flatMap(List(1,2,3))(i => List(i,i)))

    //Exercise 3.21
    println(List.filterViaFlatMap(List(1,2,3,4,5))(x => x % 2 == 0))

    //Exercise 3.22
    println(List.addPairWise(List(1,2,3), List(4,5,6)))

    //Exercise 3.23
    println(List.zipWith(List(1,2,3), List(4,5,6))(_ * _))
*/
    //Exercise 3.24
    println(List.hasSubsequence(List(1,2,3), List(2,3)))
  }
}