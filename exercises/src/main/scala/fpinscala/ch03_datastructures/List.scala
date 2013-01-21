package fpinscala.ch03_datastructures


import annotation.tailrec


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  val cascadingMatch = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def cascadingMatchAssert() {
    val answer: Int = ???
    assert(cascadingMatch == answer)
  }

  // Exercise 3.2

  def tail[A](l: List[A]): List[A] =
    ???

  // Exercise 3.3

  def drop[A](l: List[A], n: Int): List[A] =
    ???

  // Exercise 3.4

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    ???

  // Exercise 3.5

  def setHead[A](l: List[A])(h: A): List[A] =
    ???

  // Exercise 3.6

  def init[A](l: List[A]): List[A] =
    ???

  // Exercise 3.7

  /* Evaluation of foldRight(Cons(1, Cons(2, Cons(3, Nil))), 0)(_ + _):
   *
   *     TODO: write out the evaluation line by line
   */

  // Exercise 3.8

  /* TODO: Is short circuiting possible?
   */

  // Exercise 3.9

  def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def foldRightConsAssert() {
    val someList = List(1, 2, 3)
    val evaluated = foldRight(someList, Nil: List[Int]) { Cons(_, _) }
    val answer: List[Int] = ???
    assert(evaluated == answer)
  }

  // Exercise 3.10

  def length[A](l: List[A]): Int =
    ???

  // Exercise 3.11

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    ???

  // Exercise 3.12

  object AggregatesUsingFoldLeft {

    def sum(ints: List[Int]) =
      ???

    def product(ints: List[Double]) =
      ???

    def length[A](l: List[A]): Int =
      ???

  }

  // Exercise 3.13

  def reverse[A](l: List[A]): List[A] =
      ???

  // Exercise 3.14

  object FoldsWithFolds {

    def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
      ???

    def foldRight[A,B](l: List[A], z: B)(f: (A, B) => B): B =
      ???

  }

  // Exercise 3.15

  def append[A](l: List[A], r: List[A]): List[A] =
    ???

  // Exercise 3.16

  def concat[A](l: List[List[A]]): List[A] =
    ???

  // Exercise 3.17

  def add1(l: List[Int]): List[Int] =
    ???

  // Exercise 3.18

  def doubles2Strings(l: List[Double]): List[String] =
    ???

  // Exercise 3.19

  def map[A,B](l: List[A])(f: A => B): List[B] =
    ???

  // Exercise 3.20

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    ???

  // Exercises 3.21

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    ???

  object FilterWithFlatMap {

    // Exercises 3.22

    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      ???

  }

  // Exercises 3.23

  def addPairwise(as: List[Int], bs: List[Int]): List[Int] =
    ???

  // Exercises 3.24

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] =
    ???

  // Exercise 3.25

  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    ???

}
