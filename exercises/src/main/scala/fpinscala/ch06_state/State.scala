package fpinscala.ch06_state


import annotation.tailrec


trait RNG {

  def nextInt: (Int, RNG)

}


object RNG {

  def simple(seed: Long): RNG = new RNG {
    def nextInt = {
      // `&` is bitwise AND
      // `<<` is left binary shift
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      // `>>>` is right binary shift with zero fill
      ((seed2 >>> 16).asInstanceOf[Int], simple(seed2))
    }
  }

  // Exercise 6.1

  def positiveInt(rng: RNG): (Int, RNG) =
    ???

  // Exercise 6.2

  def double(rng: RNG): (Double, RNG) =
    ???

  // Exercise 6.3.a

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    ???

  // Exercise 6.3.b

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    ???

  // Exercise 6.3.c

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    ???

  // Exercise 6.4

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    ???

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = { _.nextInt }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  object UsingRandAndMap {

    // Exercise 6.5

    def positiveMax(n: Int): Rand[Int] =
      ???

    // Exercise 6.6

    def double: Rand[Double] =
      ???

  }

  // Exercise 6.7

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    ???

  // Exercise 6.8

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    ???

  // Exercise 6.9.a

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    ???

  object UsingFlatMap {

    // Exercise 6.9.b

    def positiveInt: Rand[Int] =
      ???

    // Exercise 6.10.a

    def map[A, B](a: Rand[A])(f: A => B): Rand[B] =
      ???

    // Exercise 6.10.b

    def map2[A, B, C](a: Rand[A])(b: Rand[B])(f: (A, B) => C): Rand[C] =
      ???

  }

}


case class State[S,+A](run: S => (A, S)) {

  // Exercise 6.11.b

  def flatMap /* try designing the signature yourself */ =
    ???

  // Exercise 6.11.c

  def map /* try designing the signature yourself */ =
    ???

  // Exercise 6.11.d

  def map2 /* try designing the signature yourself */ =
    ???

}


object State {

  // Exercise 6.11.a

  def unit /* try designing the signature yourself */ =
    ???

  // Exercise 6.11.e

  def sequence /* try designing the signature yourself */ =
    ???

  // Exercise 6.12

  def get /* try designing the signature yourself */ =
    ???

  def set /* try designing the signature yourself */ =
    ???

  /* The following implementation for 'modify' compiles upon implementation of
   * 'get' and 'set' above.
   */
  /*
  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()
  */

}


sealed trait Input
case object Coin extends Input
case object Turn extends Input


case class Machine(locked: Boolean, candies: Int, coins: Int)


object Machine {

  import State._

  // Exercise 6.13

  def simulateMachine(inputs: List[Input]): State[Machine, Int] =
    ???

}
