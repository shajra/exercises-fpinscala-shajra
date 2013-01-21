package fpinscala.ch02_structuringprograms


import annotation.tailrec


case class Box(height: Double, width: Double)


object Exercises {

  def greaterBy(x: Box, y: Box, f: Box => Double): Box =
    if (f(x) > f(y)) x else y

  type Pred[A] = A => Boolean

  // Exercise 2.1.a

  def taller(x: Box, y: Box): Box =
    ???

  // Exercise 2.1.b

  def wider(x: Box, y: Box): Box =
    ???

  object Absolute {

    object IntSpecific {

    // Exercise 2.2

      def absolute(f: Int => Int): Int => Int =
        ???

    }

    object Polymorphic {

      // Exercise 2.3

      def absolute[A](f: A => Int): A => Int =
        ???

    }

  }

  // Exercise 2.4

  def divisibleBy(k: Int): Pred[Int] =
    ???

  // Exercise 2.5

  def even: Pred[Int] =
    ???

  // Exercise 2.6


    object Naive {

      // Exercise 2.6.a

      def divisibleBy3And5: Pred[Int] =
        ???

      // Exercise 2.6.b

      def divisibleBy3Or5: Pred[Int] =
        ???

    }

    object WithLift {

      // Exercise 2.6.c

      def lift[A]
          (f: (Boolean, Boolean) => Boolean,
           g: Pred[A],
           h: Pred[A])
          : Pred[A] =
        ???

      // Exercise 2.6.d

      def divisibleBy3And5: Pred[Int] =
        ???

      // Exercise 2.6.e

      def divisibleBy3Or5: Pred[Int] =
        ???

    }

  // Exercise 2.7

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    ???

  // Exercise 2.8

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    ???

  // Exercise 2.9

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    ???

  object Lift3 {

    object Naive {

      // Exercise 2.10

      def lift3[A,B,C,D,E]
          (f: (B, C, D) => E)
          (g: A => B, h: A => C, i: A => D)
          : A => E =
        ???

    }


    object ReusingLift {

      // Exercise 2.11.a

      def lift[A,B,C,D]
          (f: (B, C) => D)
          (g: A => B, h: A => C)
          : A => D =
        ???

      // Exercise 2.11.b

      def lift3[A,B,C,D,E]
          (f: (B, C, D) => E)
          (g: A => B, h: A => C, i: A => D)
          : A => E =
        ???

    }

  }

  object Fib {

    object Naive {

      // Exercise 2.12.a

      def fib(n: Int): Int =
        ???

    }

    object TailRecursive {

      // Exercise 2.12.b

      def fib(n: Int): Int =
        ???

    }

  }

  def sqrt(n: Double): Double = {

    // We want to find the `x` such that `x` squared minus `n` equals `0`.
    def f(x: Double) = (x * x) - n

    iterateWhile(2.0)(
      // Starting with a guess of `2.0`, iteratively improve the guess.
      x => x - f(x) / (2 * x),
      // `1e-14` is a way of writing `10` to the `-14`th power, a rather small
      // number. When the difference between the guess and the answer is
      // smaller than this, the guess is "good enough".
      x => f(x).abs > 1e-14)

  }

  // Exercise 2.13

  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
    ???

}
