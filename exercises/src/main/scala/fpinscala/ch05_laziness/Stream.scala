package fpinscala.ch05_laziness


import annotation.tailrec

import Stream._


trait Stream[+A] {

  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    uncons match {
      case Some((h, t)) => f(h, t.foldRight(z)(f))
      case None => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.1

  def toList: List[A] =
    ???

  object SimpleTakes {

    // Exercise 5.2

    def take(n: Int): Stream[A] =
      ???

    // Exercise 5.3

    def takeWhile(p: A => Boolean): Stream[A] =
      ???

  }

  // Exercise 5.4

  def forAll(p: A => Boolean): Boolean =
    ???

  object UsingFoldRight {

    // Exercise 5.5

    def takeWhile(p: A => Boolean): Stream[A] =
      ???

    // Exercise 5.6.a

    def map /* try designing the signature yourself */ =
      ???

  }

  // Exercise 5.6.b

  def filter /* try designing the signature yourself */ =
    ???

  // Exercise 5.6.c

  def append /* try designing the signature yourself */ =
    ???

  // Exercise 5.6.d

  def flatMap /* try designing the signature yourself */ =
    ???

  // Exercise 5.12.a

  def map /* use the signature from Exercise 5.6 */ =
    ???

  // Exercise 5.12.b

  def take(n: Int): Stream[A] =
    ???

  // Exericse 5.12.c

  def takeWhile(p: A => Boolean): Stream[A] =
    ???

  // Exericse 5.12.d

  def zip /* try designing the signature yourself */ =
    ???

  // Exericse 5.12.e

  def zipAll /* try designing the signature yourself */ =
    ???

  // Exercise 5.14

  def tails: Stream[Stream[A]] =
    ???

  // Exercise 5.15

  def scanRight /* try designing the signature yourself */ =
    ???

  object UsingScanRight {

    def tails: Stream[Stream[A]] =
      ???

  }

}


object Stream {

  def empty[A]: Stream[A] =
    new Stream[A] { def uncons = None }

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
    new Stream[A] {
      lazy val uncons = Some((hd, tl))
    }

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] =
    cons(1, ones)

  // Exercise 5.7

  def constant[A](a: A): Stream[A] =
    ???

  // Exercise 5.8

  def from(n: Int): Stream[Int] =
    ???

  // Exercise 5.9

  def fibs: Stream[Int] =
    ???

  // Exercise 5.10

  def unfold[A, S](s: S)(f: S => Option[(A, S)]): Stream[A] =
    ???

  object UsingUnfold {

    // Exercise 5.11.a

    def fibs: Stream[Int] =
      ???

    // Exercise 5.11.b

    def from(n: Int): Stream[Int] =
      ???

    // Exercise 5.11.c

    def constant[A](a: A): Stream[A] =
      ???

    // Exercise 5.11.d

    def ones: Stream[Int] =
      ???

  }

  // Exercise 5.13

  def startsWith[A](as: Stream[A], prefix: Stream[A]): Boolean =
      ???

}
