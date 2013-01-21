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

  def boolean(rng: RNG) = 
    rng.nextInt match { case (i, rng2) => (i % 2 == 0, rng2) }

  // Exercise 6.1

  def positiveInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    // `Int.MinValue` is a corner case that needs special handling since its
    // absolute value doesn't fit in an `Int`.  We could just select
    // `Int.MaxValue` or `0` as a replacement but that would skew the
    // generator.  One solution is to simply retry recursively until we get a
    // different number.
    if (i == Int.MinValue) positiveInt(r) else (i.abs, r)
  }

  // Exercise 6.2

  def double(rng: RNG): (Double, RNG) = {
    val (i, r) = positiveInt(rng)
    // We generate a positive integer and divide it by one higher than the
    // maximum.  This is just one possible solution.
    (i / (Int.MinValue.toDouble + 1), r)
  }

  // Exercise 6.3.a

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, r1) = positiveInt(rng)
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  // Exercise 6.3.b

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  // Exercise 6.3.c

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  // Exercise 6.4

  object Ints {

    /* We can address the repetition of passing the RNG along every time with
     * recursion.
     */

    object NonTailRecursion {

      def ints(count: Int)(rng: RNG): (List[Int], RNG) =
        if (count <= 0)
          (List(), rng)
        else {
          val (i, r1) = rng.nextInt
          val (is, r2) = ints(count - 1)(r1)
          (i :: is, r2)
        }

    }

    object TailRecursion {

      def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
        @tailrec
        def ints(c: Int, acc: List[Int], r0: RNG): (List[Int], RNG) =
          if (c <= 0)
            (acc, r0)
          else {
            val (i, r1) = r0.nextInt
            ints(c - 1, i :: acc, r1)
          }
        ints(count, List(), rng)
      }

    }

  }

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
      map(positiveInt) { _ % (n + 1) }

    // Exercise 6.6

    def double: Rand[Double] =
      map(positiveInt) { _ / (Int.MinValue.toDouble + 1) }

  }

  // Exercise 6.7

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  /* This implementation of 'map2' passes the initial RNG to the first argument
   * and the resulting RNG to the second argument.  It's not necessarily wrong
   * to do this the other way around, since the results are random anyway.  We
   * could even pass the initial RNG to both `f` and `g`, but that might have
   * unexpected results.  For example, if both arguments are `RNG.int` then we
   * would always get two of the same Int in the result.  When implementing
   * functions like this, it's important to consider how we would test them for
   * correctness.
   */

  // Exercise 6.8

  object Sequence {

    object LowLevel {

      /* This works, but doesn't take advantage of the 'map2' combinator. */

      def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
        rng => {
          fs.foldRight((List[A](), rng)) { case (ra, (acc, r)) =>
            val (a, r1) = ra(r)
            (a :: acc, r1)
          }
        }

    }

    object UsingMap2 {

      def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
        fs.foldRight(unit(List[A]())) { map2(_, _) { _ :: _ } }

      /* Using 'map2' has a clear payoff.  Also, it's interesting that we never
       * actually need to talk about the `RNG` value in `sequence`.  This is a
       * strong hint that we could make this function polymorphic in that type.
       */

    }

  /* We are using `foldRight` for these implementations of 'sequence', which as
   * we've discussed risks overflowing the stack for sufficiently large lists.
   * If we used `foldLeft` to avoid this problem, then the values in the
   * resulting list would appear in reverse order.  It would be arguably better
   * to use `foldLeft` with `reverse`.  What do you think?
   */

  }

  // Exercise 6.9.a

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, r1) = f(rng)
      g(a)(r1)
    }

  object UsingFlatMap {

    // Exercise 6.9.b

    def positiveInt: Rand[Int] =
      flatMap(int) { i =>
        if (i == Int.MinValue) positiveInt else unit(i.abs)
      }

    // Exercise 6.10.a

    def map[A, B](a: Rand[A])(f: A => B): Rand[B] = {
      flatMap(a)(f andThen unit)
    }

    /* You may have used "{ a => unit(f(a)) }" instead of "f andThen unit".
     * 'andThen' and 'compose' are provided on Scala's Function1 to help
     * compose functions without worrying about an explicit input parameter.
     * This way of mixing functions is called "point free style".  This style
     * is similar to using pipes in POSIX shell scripting.
     *
     * Unfortunately, Scala's inference can often complicate attempts to use
     * point free style.  Sometimes it works out more elegantly and other times
     * less so.
     */

    // Exercise 6.10.b

    def map2[A, B, C](a: Rand[A])(b: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(a) { a => map(b) { b => f(a, b) } }

  }

}


case class State[S,+A](run: S => (A, S)) {

  // Exercise 6.11.b

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State { s =>
      val (a, s1) = run(s)
      f(a).run(s1)
    }

  // Exercise 6.11.c

  def map[B](f: A => B): State[S, B] =
    flatMap(f andThen State.unit)

  /* Defining 'map' in terms of 'flatMap' calls 'unit' unnecessarily, but it's
   * elegant and limits the state-specific definition to just 'flatMap' and
   * 'unit'.   Note that we've seen other abstrations like List and Option that
   * have functions like 'map', 'flatmap', 'unit', 'map2', and 'sequence'.
   * Furthermore, the implementations built from 'flatMap' and 'unit' can be
   * reused in all of these abstractions.  We've seen how to abstract away
   * State from Rand.  Later, we'll see how to abstract State further, so that
   * we can share implementations between abstractions like State, List,
   * Option, and many others.
   */

  object WithoutUsingFlatMap {

    def map[B](f: A => B): State[S, B] =
      State { s =>
        val (a, s1) = run(s)
        (f(a), s1)
      }

  }

  // Exercise 6.11.d

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap { a => sb.map { b => f(a, b) } }

}


object State {

  // Exercise 6.11.a

  def unit[S, A](a: A): State[S, A] =
    State { (a, _) }

  // Exercise 6.11.e

  object Sequence {

    object FoldRight {

      /* This solution is analogous to our solution for Exercise 6.8.  It uses
       * 'foldRight', though, so it has the potential to overflow the stack for
       * sufficiently large lists.
       */

      def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
        l.foldRight(unit[S, List[A]](List())) { (a, acc) =>
          a.map2(acc) { _ :: _ }
        }

    }

    object FoldLeftReverseInput {

      /* Every application of 'foldRight' can be easiliy transformed to an
       * application of 'reverse' followed by 'foldLeft'.  Using equational
       * reasoning (which we get from the referential transparency of pure
       * functions) we can prove to two are the same semantically.  However,
       * with 'foldLeft', though we don't have the potential to overflow the
       * stack, we've taken on the performance penalty of reversing our input
       * list.
       */

      def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
        l.reverse.foldLeft(unit[S, List[A]](List())) { (acc, a) =>
          a.map2(acc) { _ :: _ }
        }

    }

    /* Though not the case for 'sequence', for folds that return lists, there
     * are times when the output fold is smaller than the input fold.  In
     * these cases, to improve performance, it's preferable to reverse the
     * output list rather than the input -- to call 'reverse' after 'foldLeft',
     * rather than before it.
     *
     * In the following implementations, we practice this technique, even
     * though it's not necessary for 'sequence'.
     */

    object FoldLeftReverseOutput1 {

      /* It's possible to perform the list reversal after the 'foldLeft', but
       * we can no longer use 'map2' for the fold.  The problem is that 'map2'
       * threads through the state to its first argument before it's second.
       * But our list is not yet in the correct order, so we need to thread our
       * state through opposite to how 'map2' threads it.  This is what we're
       * doing below.
       */

      def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
        l.foldLeft(unit[S, List[A]](List())) { (stateAcc, stateA) =>
          State { s1 =>
            val (acc, s2) = stateAcc.run(s1)
            val (a, s3) = stateA.run(s2)
            (a :: acc, s3)
          }
        } map { _.reverse }

    }

    object FoldLeftReverseOutput2 {

      /* By inverting our call of 'foldLeft' with our 'State' constructor, we
       * get an implementation that's perhaps easier to follow in that we can
       * see the state threading through from left to right.
       *
       * Remember though, for 'sequence' we'd probably not go through this
       * hassle since the output and input lists are the same size so there's
       * no difference in performance.  Reversing the input list and using
       * 'map2' is much cleaner than these attempts to reverse the output list.
       */

      def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
        State { (s: S) =>
          l.foldLeft((List[A](), s)) { case ((acc, s1), action) =>
            val (a, s2) = action.run(s1)
            (a :: acc, s2)
          }
        } map { _.reverse }

    }

    object TailRecursion {

      /* The following implementation uses a loop internally and is practically
       * an inlining of a recursive definition of 'foldLeft' into the previous
       * implementation.  Alternatively, we could have used a
       * collection.mutable.ListBuffer internally.
       *
       * But why go through that effort?  Since the JVM's JIT optimizer is
       * rather good at smart inlining, preemptive inlining seems only to
       * complicate our implementation.  It seems better to use 'foldLeft'
       * directly.
       */

      def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] = {
        @tailrec
        def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A], S) =
          actions match {
            case Nil =>
              (acc, s)
            case h :: t =>
              h.run(s) match { case (a, s2) => go(s2, t, a :: acc) }
          }
        State { (s: S) => go(s, l, List()) } map { _.reverse }
      }

    }

  }

  def sequence[S, A](l: List[State[S, A]]): State[S, List[A]] =
    Sequence.FoldLeftReverseInput.sequence(l)

  // Exercise 6.12

  def get[S]: State[S, S] =
    State { s => (s, s) }

  def set[S](s: S): State[S, Unit] =
    State { _ => ((), s) }

  def modify[S](f: S => S): State[S, Unit] =
    for {
      s <- get
      _ <- set(f(s))
    } yield ()

}


sealed trait Input
case object Coin extends Input
case object Turn extends Input


case class Machine(locked: Boolean, candies: Int, coins: Int)


object Machine {

  import State._

  // Exercise 6.13

  def simulateMachine(inputs: List[Input]): State[Machine, Int] = {
    def step(i: Input, m: Machine): Machine =
      if (m.candies <= 0)
        m
      else
        i match {
          case Coin =>
            m.copy(coins = m.coins + 1, locked = m.candies > 0)
          case Turn =>
            if (!m.locked)
              m.copy(locked = true, candies = m.candies - 1)
            else m
        }
    for {
      _ <- sequence(inputs map { i => modify { (m: Machine) => step(i, m) } })
      m <- get
    } yield m.coins
  }

}
