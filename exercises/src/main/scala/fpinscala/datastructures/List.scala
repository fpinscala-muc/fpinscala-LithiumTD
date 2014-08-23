package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

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


  def tail[A](l: List[A]): List[A] = {
    l match {
		case Nil => sys.error("Error: Empty list passed to tail function")
		case Cons(_,t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
		case Nil => sys.error("Error: Empty list passed to setHead function")
		case Cons(_,t) => Cons(h,t)
    }
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1) 
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(h,t) if f(h) => dropWhile(t, f) 
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match { 
      case Nil => sys.error("Error: Empty list passed to init function")
      case Cons(_,Nil) => Nil
      case Cons(h,t) => Cons(h,init(t))
    }
  }

  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_,len) => len + 1)
  }

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z, h))(f)
    }
  }

  def sumViaFoldLeft(nums: List[Int]): Int = {
    foldLeft(nums, 0)(_ + _)
  }

  def productViaFoldLeft(nums: List[Double]): Double = {
    foldLeft(nums, 1.0)(_ * _)
  }

  def lengthViaFoldLeft(l: List[_]): Int = {
    foldLeft(l, 0)((len, _) => len + 1)
  }
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((h, t) => Cons(t, h))
  }

  def appendViaFoldRight[A](l1: List[A], l2: List[A]): List[A] = {
    foldRight(l1, l2)(Cons(_,_))
  }

  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = {
    foldLeft(reverse(a1), a2)((h, t) => Cons(t, h))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil:List[A])(append(_, _))
  }

  def add1[T:Numeric](nums: List[T])(implicit ev: Numeric[T]): List[T] = ???

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((h, t) => Cons(h.toString, t))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

  def filter[A](l: List[A])(f: A => Boolean): List[A] = sys.error("todo")

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = sys.error("todo")

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] = sys.error("todo")

  def addPairwise(a: List[Int], b: List[Int]): List[Int] = sys.error("todo")

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = sys.error("todo")

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean = sys.error("todo")
}