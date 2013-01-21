package fpinscala.ch02_structuringprograms


import annotation.tailrec


case class Box(height: Double, width: Double)


object Exercises {

  def greaterBy(x: Box, y: Box, f: Box => Double): Box =
    if (f(x) > f(y)) x else y

  type Pred[A] = A => Boolean

  // Exercise 2.1.a

  def taller(x: Box, y: Box): Box = greaterBy(x, y, _.height)

  // Exercise 2.1.b

  def wider(x: Box, y: Box): Box = greaterBy(x, y, _.width)

  object Absolute {

    object IntSpecific {

    // Exercise 2.2

      def absolute(f: Int => Int): Int => Int =
        f(_).abs  // uses the built-in 'abs' method on 'Int'

    }

    object Polymorphic {

      // Exercise 2.3

      def absolute[A](f: A => Int): A => Int = f(_).abs

    }

  }

  // Exercise 2.4

  def divisibleBy(k: Int): Pred[Int] = _ % k == 0

  // Exercise 2.5

  def even: Pred[Int] = divisibleBy(2)

  // Exercise 2.6


    object Naive {

      // Exercise 2.6.a

      def divisibleBy3And5: Pred[Int] =
        n => divisibleBy(3)(n) && divisibleBy(5)(n)

      // Exercise 2.6.b

      def divisibleBy3Or5: Pred[Int] =
        n => divisibleBy(3)(n) || divisibleBy(5)(n)

    }

    object WithLift {

      // Exercise 2.6.c

      def lift[A]
          (f: (Boolean, Boolean) => Boolean,
           g: Pred[A],
           h: Pred[A])
          : Pred[A] =
        x => f(g(x), h(x))

      // Exercise 2.6.d

      def divisibleBy3And5: Pred[Int] =
        lift(_ && _, divisibleBy(3), divisibleBy(5))

      // Exercise 2.6.e

      def divisibleBy3Or5: Pred[Int] =
        lift(_ || _, divisibleBy(3), divisibleBy(5))

    }

  /* Calling `divisibleBy(0)` results in an error, but we get different results
   * for these two expressions:
   *
   *     lift(_ || _, divisibleBy(2), divisibleBy(0))
   *
   *     (n: Int) => divisibleBy(2)(n) || divisibleBy(0)(n)
   *
   * Try them with different inputs.  Why do you think one of them fails with
   * an error for even numbers and the other one just returns `true` without
   * failing?  Do you think this has any implications for referential
   * transparency?  Make a note of your thoughts and revisit this question
   * after reading the chapter on strictness and laziness.
   */

  // Exercise 2.7

  def curry[A,B,C](f: (A, B) => C): A => B => C =
    a => b => f(a, b)

  /* Scala's 'Function2' trait has a 'curried' method, but defining our own is
   * more instructive:
   *
   *     def curry[A,B,C](f: (A, B) => C): A => B => C =
   *       f.curried
   */

  // Exercise 2.8

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    f(_)(_)

  /* There is a 'uncurried' method on the `Function` object in the standard
   * library that you can use for uncurrying.
   */

  /* We can go back and forth between the curried and uncurried forms, which
   * are in some sense "the same."  In FP jargon, we say that they are
   * _isomorphic_ ("iso" meaning same and "morphe" meaning shape or form), a
   * term the programming community has adopted from mathematics.
   */

  // Exercise 2.9

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    x => f(g(x))

  object Lift3 {

    object Naive {

      // Exercise 2.10

      def lift3[A,B,C,D,E]
          (f: (B, C, D) => E)
          (g: A => B, h: A => C, i: A => D)
          : A => E =
        a => f(g(a), h(a), i(a))

    }

    object ReusingLift {

      // Exercise 2.11.a

      def lift[A,B,C,D]
          (f: (B, C) => D)
          (g: A => B, h: A => C)
          : A => D =
        x => f(g(x), h(x))

      // Exercise 2.11.b

      def lift3[A,B,C,D,E]
          (f: (B, C, D) => E)
          (g: A => B, h: A => C, i: A => D)
          : A => E =
        a => lift[A,B,C,E]((b, c) => f(b, c, i(a)))(g, h)(a)

    }

  }

  object Fib {

    object Naive {

      // Exercise 2.12.a

      def fib(n: Int): Int = {
        if (n < 2) n else fib(n - 1) + fib(n - 2)
      }

    }

    object TailRecursive {

      // Exercise 2.12.b

      def fib(n: Int): Int = {

        @tailrec
        def loop(m: Int, prev1: Int, prev2: Int): Int = {
          if (m < 1) prev2 else loop(m - 1, prev1 + prev2, prev1)
        }

        loop(n, 1, 0)

      }

      /* Although this tail recursive implementation is a correct and
       * appropriate solution, it is not a simple transformation of the naive
       * algorithm, which explicitly uses the binary recursion that many people
       * are familiar with when defining the Fibonacci series:
       *
       *     fib(n) = fib(n-1) + fib(n-2), given fib(0) = 0 and fib(1) = 1
       *
       * Our tail recursive solution uses the fact that n-2 is right behind n-1
       * to reduce away the binary recursion to the simple kind of recursion
       * that Scala can optimize away.
       */

      /* Also, it's common practice to annotate functions you expect to be
       * tail recursive with the `tailrec` annotation.  If the function is not
       * tail recursive, it will yield a compile error, rather than silently
       * compiling the code and resulting in greater stack space usage at
       * runtime.
       */

    }

    object Cps {

      /* It is possible to transform any non-tail recursion into a
       * tail recursive algorithm using continuation passing style (CPS).  This
       * transformation is route, and you can still see the original algorithm
       * once you're acclimated to the style.
       *
       * The trick with CPS is to pass in as a second argument a "continuation"
       * function that is used to process all results.  We start with the
       * identity function for this continuation.
       *
       * Unfortunately, Scala's implementation of tail call optimization is
       * extremely naive and does not deal with the CPS transformation of
       * binary recursion, even though the result is clearly tail recursive
       * (try installing an @tailrec annotation and you should see the
       * compilation fail).  Many other functional programming languages can
       * perform the optimization effectively.
       */

      def fib(n: Int): Int = {

        def cps(m: Int)(cont: Int => Int): Int = {
          if (m < 2)
            cont(m)  // making sure to call cont upon returning
          else
            cps(m-1) { ans1 =>  // ans1 = fib(m-1)
              cps(m-2) { ans2 =>  // ans2 = fib(m-2)
                cont(ans1 + ans2)  // again, calling cont upon returning
              }
            }
        }

        cps(n)(identity)

      }

    }

    object Trampoline {

      /* Fortunately, a third-party library called Scalaz provides a Trampoline
       * abstraction that does not overflow the stack and is a simple route
       * transformation very similar to the CPS transformation.
       *
       * Scalaz's trampoline avoids overflowing the stack, but it will probably
       * incur some loss of performance because of the extra levels of
       * indirection through closures on the heap, which the JVM is not yet
       * tuned to optimize.
       *
       * For Fibonacci, we may not use a trampoline, because it is not
       * difficult to write a specialized tail recursive algorithm that Scala
       * can optimize.  However, this is not possible for all algorithms, and
       * in these cases a trampoline can be extremely useful.
       */

      def fib(n: Int): Int = {

        import scalaz.Free.{Trampoline, return_}  // for Trampoline
        import scalaz.Scalaz.function0Instance  // for implicit conversion

        def tramp(m: Int): Trampoline[Int] = {
          if (m < 2) return_(m)
          else
            tramp(m-1).flatMap { ans1 =>
              tramp(m-2).map { ans2 =>
                ans1 + ans2
              }
            }
        }

        tramp(n).run

      }

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

  @tailrec
  def iterateWhile[A](a: A)(f: A => A, p: Pred[A]): A =
    if (p(a)) iterateWhile(f(a))(f, p) else a

}
